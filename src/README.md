A single source file is used for all machines.

# SELECT THE TARGET MACHINE

Open the ```mandelbr8.asm``` file. At the beginning, you will find the following lines:

```asm
; Enable only the build you need (set to 1).
BUILD_C64  = 1 ; Commodore 64 (or C128 in C64 mode).
BUILD_C128 = 0 ; Commodore 128
BUILD_TED  = 0 ; Commodore TED machines: Plus/4 and C16 with 64 KB.
```

Enable **only** one build at a time.

# BUILD THE BINARY

Build with TASS:

```64tass -o "mandelbr8.prg" -L "mandelbr8.lst" -a "mandelbr8.asm"```

Replace your C64 ROM with the one you just programmed.

# LICENSE

Creative Commons, CC BY

https://creativecommons.org/licenses/by/4.0/deed.en

Please add a link to this github project.

# THIRD-PARTY CODE

This program uses the ```smult16.a``` fast 6502 multiplication algorithm from:
https://github.com/TobyLobster/multiply_test

