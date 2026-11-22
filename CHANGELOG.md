# Changelog

All notable changes to C-BIOS are listed here, one line per change. The
format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Every pull request that changes behaviour adds its line under
**Unreleased**; at release time that section gets the version number and
date.

## [Unreleased]

### Fixed
- INITIO set PSG registers 11 and 15 to other values
- The tape timing tables (CS120, CS240 and the current speed at $F406) were 0 after power-on
- TAPION, TAPIN and TAPIOF were stubs: reading from tape always failed; they now read tapes at any rate (1200 and 2400 baud, and .cas files in openMSX)
- TAPOON, TAPOUT and TAPOOF were stubs: writing to tape always failed; they now write at the rate of the current speed, 1200 or 2400 baud
- STMOTR did nothing for 2-$7E and toggled only for $FF: now 0 stops the motor, any other value without bit 7 starts it and any with bit 7 toggles it; it also returned other values in A and F
- GICINI changed registers A, E and F
- GICINI did not reset the PLAY queues, MUSICF, PLYCNT or the voice buffers
- BEEP, and so CHPUT's BEL (CTRL+G), made no sound; BEEP now sounds a short tone and then resets the sound system as GICINI does
- The function keys typed nothing: FNKSTR was empty after power-on, and INIFNK did not write the default key texts
- SHIFT+F1 to F5 typed the texts of F1 to F5 instead of F6 to F10
- DSPFNK, ERAFNK and FNKSB did nothing; they now draw and erase the function key line, scrolling the text up first when the cursor is on its row
- INITXT, INIT32 and CHGMOD did not draw the function key line when it was on
- With the function key line on, CHPUT and INLIN ran onto its row: LF, wraps, cursor down and ESC B, J, L and M now stop on the row above it
- CLS, and so CHPUT's CLS, ESC E and ESC j, left the function key line off the screen when it was on
- STRTMS did nothing; it now starts the queued PLAY music in MUSICF, PLYCNT and the voice buffers
- Queued PLAY music (PUTQ, then STRTMS) never sounded: there was no background music player; the timer interrupt now plays the three voices' queues
- CALBAS printed an error and never returned; it now calls the routine at IX in the main ROM's slot
- CALSLT, CALLF and CALBAS always returned with interrupts off; they now leave them as the called routine left them
- CHRGTR returned the character at HL instead of the next one, skipped no LF, and returned other flags
- SYNCHR was a stub: it returned without checking the character and ran its inline byte as code; it now checks it and goes on as CHRGTR, and a mismatch is a Syntax error
- GICINI, and so INITIO, set up the PLAY queues at their default place even when QUEUES pointed elsewhere
- GTTRIG read the space bar for 5 and up; now 5 to $80 read a joystick trigger (button B, port 1 for odd values, port 2 for even ones) and $81 to $FF the space bar
- GTSTCK read joystick port 2 for every value from 3 up; now 3 to $80 read port 1 and $81 to $FF the cursor keys
- OUTDLP and OUTDO sent the graphic header and graphic characters as they are to a printer that is not an MSX printer; they now hold back the header and send a space for the graphic character, and return LPTOUT's flags
- CHPUT's cursor right from the last column of the function key line's row went to the last column of the bottom text row instead of its first column
- INITIO returned other values in A, E and F
- LPTSTT returned other flags
- CNVCHR returned the Z flag the wrong way round
- CHSNS did not return the number of keys waiting in A
- CHGET could return with interrupts disabled
- CHGET returned other flags
- CHGET did not end a STOP pause (INTFLG 4)
- Clearing a text screen left other values in LINTTB
- POSIT accepted a column or row outside the screen
- POSIT moved the text cursor in graphic screen modes
- On MSX1, RDVRM returned the H flag cleared
- FILVRM returned other flags
- LDIRMV and LDIRVM returned other flags in SCREEN 0-3
- CALPAT returned other flags
- CHGCLR wrote the border colour instead of the text colours when SCRMOD held an invalid value from $81 to $FF
- The screen mode calls set wrong VDP base registers for table addresses that are not a multiple of the table size
- On MSX1, the screen mode calls kept other bits of VDP register 0
- On MSX1, WRTVDP with bit 7 of the register number set updated RG0SAV-RG7SAV
- On MSX2, WRTVDP with bit 7 of the register number set updated the register's copy in RAM
- On MSX2, changing screen mode kept bits 7 and 5 of RG1SAV instead of clearing bit 7 and turning the VDP interrupt on
- On MSX1, INIGRP and INIMLT overwrote NAMBAS and CGPBAS, the text modes' table addresses
- INITXT overwrote PATBAS and ATRBAS, the sprite table addresses
- KILBUF emptied the key buffer at the wrong end
- PINLIN, INLIN and QINLIN never returned; Enter now returns the line from the screen in BUF, and BS, DEL, INS and CTRL+B/E/F/N/U edit it; CTRL+STOP ends it with an empty line
- PNTINI did nothing; it now sets the colour PAINT stops at, BRDATR
- ISFLIO tested only the low byte of PTRFIL, and returned the H flag set
- OUTDO printed to the screen whatever PRTFLG said, and the H_OUTD hook held a jump to CHPUT instead of a RET
- OUTDLP printed nothing; it now sends to the printer, with TAB as spaces and the column in LPTPOS
- ISCNTC and CKCNTC returned other values in A and F, and left interrupts off
- The SELECT key produced no character
- BREAKX did not empty the key buffer or mark STOP in OLDKEY when it found CTRL+STOP
- SNSMAT returned other values in C and F
- GTTRIG returned other flags
- CHGCAP returned other values in A and F
- TRGFLG reported the joystick triggers as pressed
- CHGSND returned the H flag cleared when switching the click off
- GETVCP and GETVC2 returned other flags for voices 1-3
- LFTQ miscounted queues whose size is not a power of two minus one, and returned other values in B and C
- PUTQ stored the byte one place early, and could overfill a queue at the end of its buffer
- CHPUT: DEL blanked the character under the cursor instead of the one to its left, did nothing at the start of a row, and scrolled the screen in the bottom-right corner
- CHPUT: ESC l moved the cursor to the start of the line
- CHPUT: ESC Y put the cursor off the screen for a row or column out of range
- CHPUT: rows blanked by ESC K, l, J, L, M or a scroll got other values in LINTTB
- Clearing SCREEN 2 cleared the patterns from GRPCGP rounded down to a multiple of 256
- SETC in SCREEN 2 used only the low 4 bits of ATRBYT to recognise the background colour and to make it the background colour
- On MSX1, MAPXYC took X and Y as pixels in every mode but SCREEN 3, instead of as SCREEN 3 blocks in every mode but SCREEN 2
- SETC and NSETCX in SCREEN 3 masked ATRBYT with CMASK instead of ORing it in whole
- CLRSPR did nothing in SCREEN 0
- CLRSPR did not write RG1SAV to VDP register 1
- CLRSPR cleared the sprite patterns after the attributes, wiping attributes that overlap the pattern table
- On MSX1, CLRSPR set up sprite mode 2 when SCRMOD held 4 or more
- SCREEN 4 to 8 left the sprite colour table as it was, and SCREEN 4 set up its sprites as in SCREEN 1 to 3 with ATRBAS at $1B00 instead of $1E00
- INIGRP, INIMLT, CLS and (on MSX1) INIT32 wrote their VRAM tables in another order, which shows when the tables overlap
- Clearing SCREEN 3 also cleared the 512 bytes of VRAM after its pattern table
- Clearing SCREEN 3 used only the low 4 bits of BAKCLR for its fill byte
- SCREEN 0 was 39 columns wide by default instead of 37
- TOTEXT switched to SCREEN 1 whenever OLDSCR was not 0, instead of to the screen mode in OLDSCR
- On MSX1, CHGMOD ignored a screen mode above 3 instead of switching to SCREEN 3 (4 to 128) or SCREEN 0 (129 to 255)
- Clearing SCREEN 0 also cleared the VRAM after the 40-column screen: on MSX1 at any width above 39, on MSX2 at width 40
- SCANR did nothing; it now skips the PAINT border and counts and paints the run after it, in SCREEN 2, 3 and 4
- SCANL did nothing; it now counts the pixels left of the cursor up to the PAINT border and paints them together with SCANR's run, in SCREEN 2, 3 and 4
- On MSX1, SETC, READC, NSETCX and the graphics cursor moves handled SCREEN 0 and 1 partly as SCREEN 2; they now use 4x4 blocks there, as MAPXYC does
- GRPPRT drew nothing in SCREEN 3, where each glyph pixel is a 4x4 block, and did not move GRPACX in SCREEN 0, 1 and 3
- GRPPRT in SCREEN 2 (and SCREEN 4 on MSX2) drew lines and colours that differ from SETC's, wrote colours for empty glyph lines, drew past the right edge into the next row, drew nothing in SCREEN 4, and did not wrap to the next line at the right edge or ignore a start outside the screen
- GRPPRT: CR moved down 8 pixels in every screen mode instead of 32 in SCREEN 0, 1 and 3, did not wrap to the top below the screen, cleared the high byte of GRPACX, and changed F

## [0.31a] - 2026-11-12

### Changed
- connect the PSG as in openMSX 19 and newer

## [0.31] - 2026-11-11

### Added
- Paddle support (GTPDL)
- Touch pad support (GTPAD)
- Mouse and trackball support on MSX2 and MSX2+ (GTPAD, NEWPAD)
- STOREC, and MAPXYC in the sub-ROM
- Graphics cursor moves: RIGHTC, LEFTC, UPC, DOWNC, TUPC and TDOWNC
- SCALXY and GTASPC
- SETATR, READC and SETC
- NSETCX

### Changed
- The keyboard scan takes less than half the time

### Fixed
- STOP and CTRL+STOP had no effect
- Register values after RDSLT and WRSLT
- Cartridges in later slots did not start when an earlier cartridge's init routine returned
- CHGCAP switched the CAPS lamp the wrong way round
- On MSX1, SCREEN 0 left three VDP registers as the previous screen mode had set them
- Clearing the sprites (CLRSPR, and changing screen mode) also reset each sprite's X position
- MAPXYC put the graphics cursor in the wrong place, and only handled SCREEN 2

## [0.30] - 2026-11-10

### Added
- Keyboard scan entry at `$0D12`, as in the MSX1 BIOS; Buzz Off and Shark Hunter call it ([#21])
- Region and localization info in the openMSX machine configs

### Changed
- Keyboard scan and auto-repeat timing follow real machines: MSX1, MSX2/2+ at 50 Hz and at 60 Hz ([#11])
- More free ROM space below the font: MSX1 245 → 339, MSX2 48 → 156, MSX2+ 40 → 148 bytes ([#6], [#16], [#23])
- Boot screen and docs point to the new website, cbios.github.io, instead of SourceForge ([#24])

### Fixed
- INS and DEL produce their control codes ([#7])
- HOME produces `$0B`; SHIFT+HOME clears the screen ([#8])
- CTRL+key produces control codes; JP builds keep digits and punctuation, as Japanese machines do ([#9])
- Keys pressed in the first 5 seconds after power-on were lost ([#10])
- Keys could come out twice when typing fast ([#12])
- JP layout: `_` needs SHIFT, and SHIFT+0 gives no character ([#22])
- Wrapped lines stay joined when a line is deleted or the screen scrolls ([#13])
- Wrapped lines stay joined when a line is inserted (ESC L) ([#14])
- SCREEN 0 used the border colour as background colour ([#15])
- CALPAT returns the pattern offset in DE; fixes Demonia's title sprites ([#17])
- CALATR keeps A, the sprite number; fixes Demonia ([#18])
- LDIRVM and LDIRMV return BC and DE as on real MSX ([#19])
- DCOMPR returns the difference in A ([#20])

## [0.29a] - 2018-09-23

### Fixed
- Metadata in the openMSX configs for EU machines

## [0.29] - 2018-02-18

### Changed
- Generic international ROM config is 60 Hz; new EU variant at 50 Hz

### Removed
- NLMSX configs

Older releases are described in `doc/cbios.txt` and `doc/changelog-old.txt`.

[Unreleased]: https://github.com/cbios/cbios/compare/v0.31a...master
[0.31a]: https://github.com/cbios/cbios/compare/v0.31...v0.31a
[0.31]: https://github.com/cbios/cbios/compare/v0.30...v0.31
[0.30]: https://github.com/cbios/cbios/compare/v0.29a...v0.30
[0.29a]: https://github.com/cbios/cbios/compare/v0.29...v0.29a
[0.29]: https://github.com/cbios/cbios/releases/tag/v0.29
[#6]: https://github.com/cbios/cbios/pull/6
[#7]: https://github.com/cbios/cbios/pull/7
[#8]: https://github.com/cbios/cbios/pull/8
[#9]: https://github.com/cbios/cbios/pull/9
[#10]: https://github.com/cbios/cbios/pull/10
[#11]: https://github.com/cbios/cbios/pull/11
[#12]: https://github.com/cbios/cbios/pull/12
[#13]: https://github.com/cbios/cbios/pull/13
[#14]: https://github.com/cbios/cbios/pull/14
[#15]: https://github.com/cbios/cbios/pull/15
[#16]: https://github.com/cbios/cbios/pull/16
[#17]: https://github.com/cbios/cbios/pull/17
[#18]: https://github.com/cbios/cbios/pull/18
[#19]: https://github.com/cbios/cbios/pull/19
[#20]: https://github.com/cbios/cbios/pull/20
[#21]: https://github.com/cbios/cbios/pull/21
[#22]: https://github.com/cbios/cbios/pull/22
[#23]: https://github.com/cbios/cbios/pull/23
[#24]: https://github.com/cbios/cbios/pull/24
