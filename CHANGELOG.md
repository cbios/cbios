# Changelog

All notable changes to C-BIOS are listed here, one line per change. The
format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Every pull request that changes behaviour adds its line under
**Unreleased**; at release time that section gets the version number and
date.

## [Unreleased]

## [0.30] - 2026-10-31

### Added
- Keyboard scan entry at `$0D12`, as in the MSX1 BIOS; Buzz Off and Shark Hunter call it ([#21])
- Region and localization info in the openMSX machine configs

### Changed
- Keyboard scan and auto-repeat timing follow real machines: MSX1, MSX2/2+ at 50 Hz and at 60 Hz ([#11])
- More free ROM space below the font: MSX1 245 → 339, MSX2 48 → 156, MSX2+ 40 → 148 bytes ([#6], [#16], [#23])
- Boot screen and docs point to github.com/cbios instead of SourceForge ([#24])

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

[Unreleased]: https://github.com/cbios/cbios/compare/v0.30...master
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
