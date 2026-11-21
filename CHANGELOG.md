# Changelog

All notable changes to C-BIOS are listed here, one line per change. The
format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Every pull request that changes behaviour adds its line under
**Unreleased**; at release time that section gets the version number and
date.

## [Unreleased]

### Fixed
- INITIO set PSG registers 11 and 15 to other values
- GICINI changed registers A, E and F
- INITIO returned other values in A, E and F
- LPTSTT returned other flags
- CNVCHR returned the Z flag the wrong way round
- CHSNS did not return the number of keys waiting in A
- CHGET could return with interrupts disabled
- CHGET returned other flags
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
- On MSX1, INIGRP and INIMLT overwrote NAMBAS and CGPBAS, the text modes' table addresses
- INITXT overwrote PATBAS and ATRBAS, the sprite table addresses
- KILBUF emptied the key buffer at the wrong end
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
- Clearing SCREEN 3 also cleared the 512 bytes of VRAM after its pattern table
- Clearing SCREEN 3 used only the low 4 bits of BAKCLR for its fill byte
- SCREEN 0 was 39 columns wide by default instead of 37
- TOTEXT switched to SCREEN 1 whenever OLDSCR was not 0, instead of to the screen mode in OLDSCR
- On MSX1, CHGMOD ignored a screen mode above 3 instead of switching to SCREEN 3 (4 to 128) or SCREEN 0 (129 to 255)
- Clearing SCREEN 0 also cleared the VRAM after the 40-column screen: on MSX1 at any width above 39, on MSX2 at width 40

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
