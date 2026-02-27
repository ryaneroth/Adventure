# Colossal Cave Adventure for the KIM-1

A port of [Colossal Cave Adventure](https://github.com/troglobit/adventure) to the [MOS Technology KIM-1](https://en.wikipedia.org/wiki/KIM-1) single-board computer, written in 6502 assembly.

## Overview

This project brings the classic 1976 text adventure game to the KIM-1. Game data is stored on an SD card (FAT32) and streamed at runtime, working around the KIM-1's limited RAM. Input and output are handled over the KIM-1's serial interface.

## Hardware Requirements

- MOS Technology KIM-1 (or compatible 6502 SBC)
- SD card interface connected to the KIM-1's VIA (6522):
  - CS   → Port A bit 4
  - SCK  → Port A bit 3
  - MOSI → Port A bit 2
  - MISO → Port A bit 1
- SD card formatted with FAT32, containing the game data files
- Serial terminal for input/output

## Memory Map

| Address       | Contents                          |
|---------------|-----------------------------------|
| `$0040–$0077` | Zero page — SD/FAT32 variables    |
| `$0200–$03FF` | FAT32 workspace                   |
| `$0700–$09C7` | Game state (snapshot block 1)     |
| `$2000–$2063` | Object place table (snapshot block 2) |
| `$2200–$254F` | Packed save/restore staging buffer |
| `$A000+`      | Program (ROM/load address)        |

## Building

### Prerequisites

- [cc65](https://cc65.github.io/) (`ca65` assembler, `ld65` linker)
- [srec_cat](https://srecord.sourceforge.net/) (for generating the `.ptp` paper tape image)

### Build

```sh
cd src
make
```

This produces:

| File         | Description                                      |
|--------------|--------------------------------------------------|
| `advent.bin` | Raw binary, offset `$A000`                       |
| `advent.ptp` | MOS Technologies paper tape format for the KIM-1 |
| `advent.lst` | Assembly listing                                 |
| `advent.map` | Linker map                                       |

### Clean

```sh
make clean
```

## Loading onto the KIM-1

Load `advent.ptp` using the KIM-1's paper tape interface or a compatible serial loader. The program starts at `$A000`.

## SD Card Setup

Format an SD card as FAT32 and copy the game data files to the root directory. The game reads location descriptions, travel tables, messages, and other data files at runtime.

## KIM-1 ROM Routines Used

| Label    | Address | Purpose              |
|----------|---------|----------------------|
| `OUTCH`  | `$1EA0` | Print a character    |
| `GETCH`  | `$1E5A` | Read a character     |
| `PRTBYT` | `$1E3B` | Print a hex byte     |
| `EXIT`   | `$1C4F` | Return to KIM-1 monitor |

## Save / Restore

The game supports saving and restoring state. A snapshot packs `$0328` bytes of game state into a staging buffer at `$2200` and writes it to the SD card.

## Credits

- Original *Colossal Cave Adventure* by Will Crowther and Don Woods
- C source and game data files (`data/`) from [troglobit/adventure](https://github.com/troglobit/adventure)
- SD card / FAT32 library: [ryaneroth/sdcard6502](https://github.com/ryaneroth/sdcard6502)
- 6502 port and testing assisted by [OpenAI Codex](https://openai.com/codex)
- 6502 port for the KIM-1 by Ryan Roth
