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

Format an SD card as FAT32 (partition type 0x0c) and copy the game data files to the root directory. The game reads location descriptions, travel tables, messages, and other data files at runtime.

[sd.zip](https://github.com/ryaneroth/Adventure/blob/main/sd.zip) is a compressed disk image, created by dd, of a working SD card.

## KIM-1 ROM Routines Used

| Label    | Address | Purpose              |
|----------|---------|----------------------|
| `OUTCH`  | `$1EA0` | Print a character    |
| `GETCH`  | `$1E5A` | Read a character     |
| `PRTBYT` | `$1E3B` | Print a hex byte     |
| `EXIT`   | `$1C4F` | Return to KIM-1 monitor |

## Save / Restore

The game supports saving and restoring state. A snapshot packs `$0328` bytes of game state into a staging buffer at `$2200` and writes it to the SD card. All scoring state variables are within the snapshot range and are saved/restored automatically.

## Scoring

Scoring follows the original *Colossal Cave Adventure* formula. Type `SCORE` at any point during the game to see your current standing.

### Score Breakdown (maximum: 350 points)

| Component | Points | Condition |
|-----------|--------|-----------|
| Treasure located | +2 each | First time you find a treasure |
| Treasure returned (standard) | +10 each | Objects 50–54 deposited at the building |
| Treasure chest returned | +12 | Pirate's chest deposited at the building |
| Treasure returned (special) | +14 each | Objects 56–64 deposited at the building |
| Magazine at Witt's End | +1 | Magazine left at its original location |
| Baseline | +2 | Always |
| Survival | 10 × (3 − deaths) | Up to +30 for zero deaths |
| Did not quit | +4 | Game ended without typing QUIT |
| Deep cave reached | +25 | Entered a room with index ≥ 15 |
| Cave closing | +25 | All 15 treasures returned to the building |
| End-game BLAST | up to +45 | Type BLAST in the repository after cave closes |

### End-Game Sequence

When all 15 treasures are deposited at the building, the cave *closes*: the player is teleported to the Repository (room 115) and the `closed` flag is set. From there, typing `BLAST` ends the game with a bonus of 10–45 points depending on the player's exact position and whether the black rod with a rusty star (ROD2) is present:

| Condition | Bonus |
|-----------|-------|
| Entered repository, did not BLAST | +10 |
| BLAST in repository, room ≠ 115, ROD2 absent | +45 |
| BLAST in room 115 (NE end), ROD2 absent | +30 |
| BLAST with ROD2 present | +25 |

### Game State Variables

| Address | Name | Purpose |
|---------|------|---------|
| `$77B` | `num_deaths` | Number of times the player has died (0–3) |
| `$77C` | `gave_up` | Set to 1 if the player used QUIT |
| `$77D` | `dflag` | Set to 1 once the deep cave is reached |
| `$77E` | `tally` | Treasures not yet returned to the building |
| `$77F` | `closing` | Set to 1 when the cave-closing event fires |
| `$8AE` | `closed` | Set to 1 once the player is teleported to the repository |
| `$8AF` | `bonus` | End-game bonus code: 0=none, 133=45 pts, 134=30 pts, 135=25 pts |

## Credits

- Original *Colossal Cave Adventure* by Will Crowther and Don Woods
- C source and game data files (`data/`) from [troglobit/adventure](https://github.com/troglobit/adventure)
- SD card / FAT32 library: [ryaneroth/sdcard6502](https://github.com/ryaneroth/sdcard6502)
- 6502 port and testing assisted by [OpenAI Codex](https://openai.com/codex) and [Anthropic Claude](https://claude.ai)
- 6502 port for the KIM-1 by Ryan Roth
