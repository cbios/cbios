; $Id: systemvars.asm,v 1.10 2004/12/22 12:29:30 andete Exp $
;
; C-BIOS system variable declarations
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
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


;-------------------
; help function area

; F380-F384: interslot read
RDPRIM:         equ     $F380

; F385-F38B: interslot read
WRPRIM:         equ     $F385

; F38C-F399: interslot call
CLPRIM:         equ     $F38C

; F39A-F3AD: workarea for the DEF USR statement
; this area is initialized with the 10 times the value $475A, which gives
; the error 'Syntax Error'
USRTAB:         equ     $F39A

;----------------------
; screen parameter area

; F3AE: # of positions on a line in SCREEN 0 (ini:39)
LINL40:         equ     $F3AE

; F3AF: # of positions on a line in SCREEN 1 (ini:29)
LINL32:         equ     $F3AF

; F3B0: # of actually used positions in the current screenmodus (ini:39)
LINLEN:         equ     $F3B0

; F3B1: # of used lines on screen (ini:24)
CRTCNT:         equ     $F3B1

; F3B2: # of positions within a tabulator-column (ini:14)
CLMLST:         equ     $F3B2

; F3B3-F3B4: BASE(0): name table address for SCREEN 0 (ini:$0000)
; used to initialize NAMBAS when SCREEN 0 is activated
TXTNAM:         equ     $F3B3

; F3B5-F3B6: BASE(1): color table address for SCREEN 0, unused? (ini:$0000)
TXTCOL:         equ     $F3B5

; F3B7-F3B8: BASE(2): pattern table address for SCREEN 0 (ini:$0800)
; used to initialize CGPBAS when SCREEN 0 is activated
TXTCGP:         equ     $F3B7

; F3B9-F3BA: BASE(3): sprite attribute table address for SCREEN 0, unused (ini:$0000)
; used to initialize ATRBAS when SCREEN 0 is activated
TXTATR:         equ     $F3B9

; F3BB-F3BC: BASE(4): sprite pattern table address for SCREEN 0, unused (ini:$0000)
; used to initialize PATBAS when SCREEN 0 is activated
TXTPAT:         equ     $F3BB

; F3BD-F3BE: BASE(5): nametable address for SCREEN 1 (ini:$1800)
; used to initialize NAMBAS when SCREEN 1 is activated
T32NAM:         equ     $F3BD

; F3BF-F3C0: BASE(6): color table address for SCREEN 1 (ini:$2000)
T32COL:         equ     $F3BF

; F3C1-F3C2: BASE(7): pattern table address for SCREEN 1 (ini:$0000)
; used to initialize CGPBAS when SCREEN 1 is activated
T32CGP:         equ     $F3C1

; F3C3-F3C4: BASE(8): sprite attribute table address for SCREEN 1 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 1 is activated
T32ATR:         equ     $F3C3

; F3C5-F3C6: BASE(9): sprite pattern table address for SCREEN 1 (ini:$0800)
; used to initialize PATBAS when SCREEN 1 is activated
T32PAT:         equ     $F3C5

; F3C7-F3C8: BASE(10): name table address for SCREEN 2 (ini:$1800)
; used to initialize NAMBAS when SCREEN 2 is activated
GRPNAM:         equ     $F3C7

; F3C9-F3CA: BASE(11): color table address for SCREEN 2 (ini:$2000)
GRPCOL:         equ     $F3C9           ; Screen2 Color

; F3CB-F3CC: BASE(12): pattern table address for SCREEN 2 (ini:$0000)
; used to initialize CGPBAS when SCREEN 2 is activated
GRPCGP:         equ     $F3CB

; F3CD-F3CE: BASE(13): sprite attribute table address for SCREEN 2 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 2 is activated
GRPATR:         equ     $F3CD

; F3CF-F3D0: BASE(14): sprite pattern table address for SCREEN 2 (ini:$3800)
; used to initialize PATBAS when SCREEN 2 is activated
GRPPAT:         equ     $F3CF

; F3D1-F3D2: BASE(15): name table address for SCREEN 3 (ini:$0800)
; used to initialize NAMBAS when SCREEN 3 is activated
MLTNAM:         equ     $F3D1

; F3D3-F3D4: BASE(16): color table address for SCREEN 3 (ini:$0000)
; the color table is unused in SCREEN 3
MLTCOL:         equ     $F3D3

; F3D5-F3D6: BASE(17): pattern table address for SCREEN 3 (ini:$0000)
; used to initialize CGPBAS when SCREEN 3 is activated
MLTCGP:         equ     $F3D5

; F3D7-F3D8: BASE(18): sprite attribute table address for SCREEN 3 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 3 is activated
MLTATR:         equ     $F3D7

; F3D9-F3DA: BASE(19): sprite pattern table address for SCREEN 3 (ini:$3800)
; used to initialize PATBAS when SCREEN 3 is activated
MLTPAT:         equ     $F3D9

; F3DB: keyclick when a key is pressed: 0: no, 1: yes
; SCREEN ,,n will write to this address
CLIKSW:         equ     $F3DB

; F3DC: line where the cursor is located
; starts to count at 1 for the topmost line
CSRY:           equ     $F3DC

; F3DD: column where the cursor is located
; starts to count at 1 for the leftmost column
CSRX:           equ     $F3DD

; F3DE: function key definition shown: 0: no, >0: yes
CNSDFG:         equ     $F3DE

; F3DF-D3E6: storage for the last written value towards VDP registers 0 till 7
; this is needed because these registers are write only
RG0SAV:         equ     $F3DF
RG1SAV:         equ     $F3E0
RG2SAV:         equ     $F3E1
RG3SAV:         equ     $F3E2
RG4SAV:         equ     $F3E3
RG5SAV:         equ     $F3E4
RG6SAV:         equ     $F3E5
RG7SAV:         equ     $F3E6
; F3E7: last read value of VDP register 8
STATFL:         equ     $F3E7

; F3E8: information about the joystick and space bar
; 7 6 5 4 3 2 1 0
; | | | |       +-- Space bar, trig(0) (0 = pressed)
; | | | +---------- Stick 1, Trigger 1 (0 = pressed)
; | | +------------ Stick 1, Trigger 2 (0 = pressed)
; | +-------------- Stick 2, Trigger 1 (0 = pressed)
; +---------------- Stick 2, Trigger 2 (0 = pressed)
TRGFLG:         equ     $F3E8

; F3E9: code for the standard foreground color (ini:15)
FORCLR:         equ     $F3E9

; F3EA: code for the standard background color (ini:4)
BAKCLR:         equ     $F3EA

; F3EB: code for the standard border color (ini:7)
BDRCLR:         equ     $F3EB

; F3EC-F3EE: Jump instruction used by Basic LINE command.
; The routines used are: RIGHTC, LEFTC, UPC and DOWNC
MAXUPD:         equ     $F3EC

; ??? this was already defined ???
PSG_DBG:        equ     $F3EC           ; デバッグ用フラグ

; F3EF-F3F1: Jump instruction used by Basic LINE command.
; The routines used are: RIGHTC, LEFTC, UPC and DOWNC
MINUPD:         equ     $F3EF

; F3F2: working color, as used for graphical operations
; normally equals to the foreground color (ini:15)
ATRBYT:         equ     $F3F2

; F3F3-F3F4: starting value of the address of the queue-table
; the queue-table contains 4 queue's: 3 for sound and one for RS232
; (ini: QUETAB ($F959))
QUEUES:         equ     $F3F3

; F3F5: CLOAD flag =0 when CLOAD =255 when CLOAD?
FRCNEW:         equ     $F3F5

; F3F6: VDP-interupt counter that counts from 3 to 0, when it reaches zero, the
; keyboard matrix is scanned, and the counters is reset at 3
SCNCNT:         equ     $F3F6

; F3F7: key repeat counter. Runs from 13 to 0, and is changed when SCNCNT is changed
; if the key remained the same. If it reaches 0, keyrepetition starts. If another key
; is pressed the value is reset at 13.
REPCNT:         equ     $F3F7

; F3F8-F3F9: first free space in the inputbuffer of the keyboard
; everytime a key is added to the inputbuffer, this address is incremented,
; when it equals to GETPNT, the buffer is full
; the buffer is located at KEYBUF
PUTPNT:         equ     $F3F8           ; キーバッファへのポインタ

; F3FA-F3FB: address in inputbuffer of first character that is not yet read
; everytime a key is read from the buffer it is incremented
; the buffer is located at KEYBUF
GETPNT:         equ     $F3FA           ; キーバッファへのポインタ

; F3FC-F400: memory area for tape system parameters for 1200 baud
; F3FC: length of  low signal for 0     (ini:83)
; F3FD: length of high signal for 0     (ini:92)
; F3FE: length of  low signal for 1     (ini:38)
; F3FF: length of high signal for 1     (ini:45)
; F400: length of synchronization block (ini:15)
CS120:          equ     $F3FC

; F401-F405: memory area for tape system parameters for 1200 baud
; F401: length of  low signal for 0     (ini:37)
; F402: length of high signal for 0     (ini:45)
; F403: length of  low signal for 1     (ini:14)
; F404: length of high signal for 1     (ini:22)
; F405: length of synchronization block (ini:31)
CS240:          equ     $F401

; F406-F407: lenghts of signal for 0 for the current speed of the tape system
; either equal to the content of F3FC-F3FD or the content of F401-F402
LOW_:           equ     $F406 ; real name: LOW, but doesn't compile?

; F408-F409: lenghts of signal for 1 for the current speed of the tape system
; either equal to the content of F3FE-F3FF or the content of F403-F404
HIGH_:          equ     $F408 ; real name: HIGH, but doesn't compile?

; F40A: lenghts of synchronization block for the current speed of the tape system
; either equal to the content of F400 or the content of F405
HEADER:         equ     $F40A

; F40B-F40C: standard setting for the height/width aspect of the
; BASIC statement CIRCLE; only the byte in F40B is actually used
; If ASPECT2 is larger then 255, the value of F40B is the number of horizontal
; dots per 256 verical dots of the radius (ini:$0100)
; ! not verified :)
ASPCT1:         equ     $F40B

; F40D-F40E: standard setting for the height/width aspect of the
; BASIC statement CIRCLE; If ASPCT2 is smaller then 512, then ASPCT2 is the
; number of vertical dots per 256 horizontal dots of the radius (ini:$0100)
; ! not verified :)
ASPCT2:         equ     $F40D

; F40F-F414: work area for the BASIC statement RESUME NEXT
; contains a fake end of basic program
ENDPRG:         equ     $F40F

; F414: errornumber of last error that happened while executing a BASIC program
; (ini:0)
ERRFLG:         equ     $F414

; F415: number of characters in the writebuffer of the printer that still
; need printing
LPTPOS:         equ     $F415

; F416: switch indicating if output should be screen or printer
; (think LIST vs LLIST) (ini:0) values: 0: screen, 1: printer
PRTFLG:         equ     $F416

; F417: switch indicating if hooked up printer is an MSX printer or not
; values: 0: MSX-Printer, 1: no MSX-Printer
NTMSXP:         equ     $F417

; F418: switch indicating of printing routines should use raw-mode or
; should convert:
; =0 to convert tabs and unknown characters to spaces and remove graphical headers
; =1 to send data just like it gets it
RAWPRT:         equ     $F418

; ---------------------------
; basic interpreter work area

; F419-F41A: work area for the BASIC command VAL: contains address of character that
; has temporarely been replaced by O by VAL
VLZADR:         equ     $F419

; F41B: work area for the BASIC command VAL: contains the character originally at
; the location of VLZADR
VLZDAT:         equ     $F41B

; F41C-F41D: line number of current BASIC line being executed, in direct modus this
; contains $FFFF (ini:$FFFF)
CURLIN:         equ     $F41C

; F41E: error detection prefix for KBUF, always contains ":"
; originally undocumented :)
KBFMIN:         equ     $F41E

; F41F-F55C: workarea for coding basic rules that have been typed in direct modus
; this are contains the code for the line interpreted in direct modus
KBUF:           equ     $F41F

STKTOP:         equ     $F674

NAMBAS:         equ     $F922
CGPBAS:         equ     $F924
PATBAS:         equ     $F926
ATRBAS:         equ     $F928

; --------------------
; filesystem work area

; ------------------------
; screen routine work area

; --------------------------------------
; work area for sound and queueing and RS232
; in the MSX2, the RS232 addresses are used for other purposes

DPPAGE:         equ     $FAF5           ; Display page (SCR5+)
ACPAGE:         equ     $FAF6           ; Active page (SCR5+)

EXBRSA:         equ     $FAF8           ; サブロム位置

; -----------------------------------------------
; settings for screen editor and interrupt system

CLIKFL:         equ     $FBD9
OLDKEY:         equ     $FBDA
NEWKEY:         equ     $FBE5
KEYBUF:         equ     $FBF0

LIMPNT:         equ     $FC17           ; キーバッファへのポインタ
LINWRK:         equ     $FC18           ; 40桁分のバッファ

BOTTOM:         equ     $FC48
HIMEM:          equ     $FC4A

ESCCNT:         equ     $FCA7           ; ESC用カウンタ.

EXP_TBL:        equ     $FCC1           ; スロット情報テーブル
SLT_TBL:        equ     $FCC5           ; スロット情報

JIFFY:          equ     $FC9E           ; timer counter
SCRMOD:         equ     $FCAF

; ---------------------------
; storage of slot information

; ------------------------------
; storage of ROM-page parameters

; ------------
; system hooks

; system hooks are defined in hooks.asm

; ------------------
; storage of VDP8-23

RG8SAV:         equ     $FFE7

; ----------------------
; extra slot information

; ---------------------------
; subslot switching addresses

; -------
; the end

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
