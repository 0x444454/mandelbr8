; Mandelbr8 (6502 version).
; DDT's fixed-point Mandelbrot 8-bit generator.
;
; https://github.com/0x444454/mandelbr8
;
; Revision history [authors in square brackets]:
;   2024-11-07: First simple test loop. [DDT]
;   2024-11-08: Unoptimized C64 version. [DDT]
;   2024-11-09: Added support for C128 and 80x50 VDC mode. [DDT]
;   2024-11-10: Added Q5.9 squares table for speedup. [DDT]
;   2024-11-12: VIC-II Kawari: Support HW multiplier. [DDT]
;   2024-11-13: Support Commodore Plus/4. [DDT]
;   2024-11-15: Support C64 multicolor bitmap (second-pass). [DDT]
;   2024-11-16: Multicolor histogram optimized rendering. [DDT]
;   2024-11-17: Added C64 tile-skip heuristics. [DDT]
;   2024-11-18: Support TED multicolor bitmap (second-pass). [DDT]
;   2024-11-20: Support Kawari custom palette. [DDT]
;   2024-11-23: Support Atari XL/XE. [DDT]
;   2024-11-27: Studied BBC Micro system arch. [DDT]
;   2024-11-28: Support BBC Micro. [DDT]
;   2024-12-02: Minor bugs fixed. [DDT]


; Enable *only* the build you need (set to 1).
BUILD_C64   = 1 ; Commodore 64 (or C128 in C64 mode).
BUILD_C128  = 0 ; Commodore 128
BUILD_TED   = 0 ; Commodore TED machines: Plus/4 and C16 with 64 KB.
BUILD_PET   = 0 ; WARNING: Commodore PET machines ARE NOT YET SUPPORTED !!!.
BUILD_ATARI = 0 ; Atari XL/XE (GTIA required).
BUILD_BEEB  = 0 ; BBC Micro.

; BEGIN: Commodore machines -----------------
.if BUILD_C64
  * = $0801   ; C64
.elif BUILD_C128
  * = $1C01   ; C128
.elif BUILD_TED
  * = $1001   ; TED machines (C16 and Plus/4)
.elif BUILD_PET
  * = $401    ; PET.
.elif BUILD_ATARI
  * = $1800   ; Atari XL/XE
.elif BUILD_BEEB
  LOAD_ADDRESS = $1800   ; BBC Micro
.endif
  
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET
  .word end_BASIC
  .word 10
  .byte $9e ; SYS

  .if BUILD_C64  
    .text "2061", $00 ; C64
  .elif BUILD_C128
    .text "7181", $00 ; C128
  .elif BUILD_TED
    .text "4109", $00 ; TED
  .elif BUILD_PET
    .text "1037", $00 ; PET
  .endif

end_BASIC:
  .word 0
.endif
; END: Commodore machines -----------------


; BEGIN: BBC Micro -----------------
.if BUILD_BEEB
        ; We generate a SSD file, i.e. a floppy disk containing this program.
        ;
        ; Create SSD disk header.
        ;
        * = LOAD_ADDRESS - $200 ;--------- SECTOR 0
        ;
        .text "mandelbr"   ; First 8 chars of disk title.
        ;    First file name and "Dir" attributes.
        .text "!boot  ", $24

        * = LOAD_ADDRESS - $100 ;--------- SECTOR 1
        .text "8   "    ; Last 4 chars of tisk title.
        .byte $07       ; Disk cycle, HDFS: Key number.
        .byte 1*8       ; (Number of catalog entries) * 8
        .byte %00100011 ; [7..6]: Must be zero.
                        ; [5..4]: !Boot option (*OPT 4 value). We use %10 to run machine code.
                        ; [3]: 0=DFS/WDFS, 1=HDFS
                        ; [2]: Total number of sectors [10]. If HDFS: (number of sides)-1
                        ; [1..0]: Total number of sectors [9..8].
        .byte ((END_ADDRESS - LOAD_ADDRESS)+255)/256 + 2 ; Total number of sectors [7..0].
        ;   First file properties.
        .byte <LOAD_ADDRESS, >LOAD_ADDRESS ; Load address [15..0].
        .byte <LOAD_ADDRESS, >LOAD_ADDRESS ; Exec address [15..0].
        .word END_ADDRESS - LOAD_ADDRESS   ; File length  [15..0].
        .byte %00000000 ; [7..6] Exec address [17..16].
                        ; [5..4] File length  [17..16].
                        ; [3..2] Load address [17..16].
                        ; [1..0] Start sector [9..8]
        .byte $02       ;        Start sector [7..0].

        * = LOAD_ADDRESS
.endif
; END: BBC Micro -----------------

        
main:
        ; Disable interrupts.
        SEI


.if BUILD_BEEB
        LDA #MODE_BEEB
        STA mode
        
        ; Mute that nasty startup beep.
        LDA #$00
        JSR set_volumes

        ; Configure System VIA Port A to read keyboard.
        LDA #$0F
        STA $FE42
        LDA #$03
        STA $FE40
        
        ; Set ULA palette for mode 2.
        ; Write the logical color to the top four bits, and the physical color EOR 7 in the bottom four bits.
        LDX #0
-       TXA
        EOR #$07
        STA ula_tmp
        TXA
        ROL
        ROL
        ROL
        ROL
        ORA ula_tmp
        STA $FE21     ; Palette Control (%ccccfbgr).
        INX
        CPX #16
        BNE -

        ; Border color / 24-bit palette control:
        ;   %0000xbgr - border colour
        ;   %01xxxxxx - palette control
        ;   %1xxxxxxx - palette control
        LDA #%00000000  ; Black.
        STA $FE22  ; Border color.
        
        
        ; Set Mode 2 (BITMAP_START = $3000).
        LDA #12    ; Set C0=1
        STA $FE40  ; VIA Reg B
        LDA #13    ; Set C1=1
        STA $FE40  ; VIA Reg B
        
        ; Setup the 6845 (CRTC).
        LDX #0
-       STX $FE00
        LDA crtc_mode_2,X
        STA $FE01
        INX
        CPX #18
        BNE -

        ; Setup Video Control register. For whatever reason, this must be done after setting up the CRTC.
        LDA #$F4   ; Mode 2 with no cursor. NOTE: setting bits 5,6 and 7 to 0 will cause the cursor to vanish.
        STA $FE20  ; Video Control.

        
        ; Draw test bitmap pattern.
        LDX #$00
-       TXA
        ;LDA #$FF
        STA BITMAP_START,X
        INX
        BNE -

;j: jmp j
;  RTS
        JMP done_BEEB
        

ula_tmp:    .byte   0  

; 6845 (CRTC) settings.
crtc_mode_2:
    .byte   127 ; R0        Horizontal total. The total number of 'character time units' across the screen - 1 (including non-displayed characters).
    .byte    80 ; R1        Characters per line. 
    .byte    98 ; R2        Horizontal sync position. Changing this moves the screen left/right.
    .byte   $28 ; R3        Vertical sync width [7..4], Horizontal sync width [3..0]. NOTE: Not advisable to change since most TVs/monitors require the standard values.
    .byte    38 ; R4        Vertical total (char rows).
    .byte     0 ; R5        Vertical total adjust (scan lines).
    .byte    32 ; R6        Vertical displayed (char rows).
    .byte    34 ; R7        Vertical sync position (char rows).
    .byte   $00 ; R8        Cursor delay (chars) [7..6], Display delay (chars) [5..4], Interlace mode [1..0]: %00 or %10 = non interlaced; %01 = interlaced; %11 = interlace and video for MODE 7 support.
    .byte     7 ; R9        Scan lines per character - 1.
    .byte   $67 ; R10       Cursor blink [6], Blink rate [5], Cursor start (scan line) [4..0].
    .byte     8 ; R11       Cursor end (scan line).
    .byte >(BITMAP_START/8); R12       Screen start address / 8, HI.
    .byte <(BITMAP_START/8); R13       Screen start address / 8, LO.
    .byte   $00 ; R14       Cursor position, HI.
    .byte   $00 ; R15       Cursor position, LO.
    .byte     0 ; R16       Light pen position, HI.
    .byte     0 ; R17       Light pen position, HLO.

; Table to convert a color index [0..15] into a byte representing 2 pixels of the same color (Mode 2).
mode2_solid_colors:
    .byte %00000000, %00000011, %00001100, %00001111
    .byte %00110000, %00110011, %00111100, %00111111
    .byte %11000000, %11000011, %11001100, %11001111
    .byte %11110000, %11110011, %11111100, %11111111

; This is the current BBC Micro joystick poll axis.
beeb_joy_dir_axis:  .byte $00
beeb_joy_last_x:    .byte 128     ; [0..255]. Default to centered.
beeb_joy_last_y:    .byte 128     ; [0..255]. Default to centered.
  
; Set volume in A to sound chip.
set_volumes:
        EOR #$0F    ; SN76489 takes a negative volume level
        PHA
        ORA #$D0    ; command: set volume of Channel 2
        JSR snd_sound_cmd
        PLA
        PHA
        ORA #$90    ; command: set volume of Channel 0
        JSR snd_sound_cmd
        PLA
        ORA #$B0    ; command: set volume of Channel 1
        JSR snd_sound_cmd
        RTS  
  
; Send command in A to sound chip.  
snd_sound_cmd:  
        LDX #$FF
        STX $FE43  ; System VIA Port A: Set all lines as output.
        STA $FE4F  ; System VIA Port A: Output A (no handshake).
        INX        ; X=0 (Enable sound chip).
        STX $FE40  ; System VIA Port B, assert sound chip write strobe.
        NOP        ; Wait for sound chip to process command...
        NOP
        NOP
        NOP
        LDX #8     ; X=8 (Disable sound chip).
        STX $FE40  ; System VIA Port B, negate sound chip write strobe.
        RTS
    
done_BEEB:    
.endif ; BUILD_BEEB


        
.if BUILD_C128
        ; Configure C128 MMU. Kick ROM out of the way.
        LDA #$3E            ; All Bank0 RAM but keep I/O regs ($D000-$DFFF).
        STA $D500
.elif BUILD_ATARI
        ; Disable IRQs.
        ;LDA #$00
        ;STA $D20E           ; IRQEN
        ; Disable NMIs
        LDA #$00
        STA $D40E           ; NMIEN

        ; Copy charset ($E000–$E3FF) from ROM to RAM.
        LDX #0
-       LDA $E000,X
        STA $1000,X
        LDA $E100,X
        STA $1100,X
        LDA $E200,X
        STA $1200,X
        LDA $E300,X
        STA $1300,X
        INX
        BNE -
        
        ; Point to copied charset.
        LDA #$10
        STA $D409           ; CHBASE
        
        ; Now disable ROMs.
        LDA $D301           ; PORTB
        AND #%11111110      ; Clear bit 0 to disable OS ROM.
        ORA #%10000010      ; Set bits 1 to disable BASIC ROM. Set bit 7 ti disable Self-Test ROM.
        STA $D301
        ; All system ROMs disabled. We still have hw regs at $D000-D7FF.
        
        ; Replace NMI vector.
        LDA #<atari_NMI
        STA $FFFA
        LDA #>atari_NMI
        STA $FFFB
        ; Replace IRQ vector.
        LDA #<atari_IRQ
        STA $FFFE
        LDA #>atari_IRQ
        STA $FFFF

        ;LDA #$00            ; Clear Pokey SKCTL
        ;STA $D20F           ; SKCTL
        ;LDA #$03            ; Then Set to $03
        ;STA $D20F           ; SKCTL

        ; Disable ANTIC DMA
        LDA #$00            ; Disable display list DMA.
        STA $D400           ; DMACTL
        
        ; Copy new display list for text (lo-res).
        LDX #0
-       LDA atari_DL_text,X
        STA $1400,X
        INX
        CMP #$41            ; This is the jump.
        BNE -
        LDA #$00            ; Set jump address.
        STA $1400,X
        LDA #$14
        STA $1401,X
        
        ; Copy new display list for bitmap (hi-res).
        LDX #0
-       LDA atari_DL_bitmap,X
        STA $1500,X
        CMP #$0F            ; Do we need to expand the template ?
        BEQ +
        INX
        CMP #$41            ; Stop at the ANTIC jump (end), in case we don't find the expansion point.
        BNE -
        BEQ done_expansion  ; If we are here, we did not find the expansion point.
+       ; Expand template and complete display list.
        LDA #$0F
        LDY #99             ; Define 99 lines before we need a LMS to hop the 4 KB boundary.
exp_0:  STA $1500,X
        INX
        DEY
        BNE exp_0
        LDA #$4F             ; Line 100: Time for a LMS.
        STA $1500,X
        INX
        LDA #$00             ; Line 100 ptr LO
        STA $1500,X
        INX
        LDA #(>BITMAP_START)+$10 ; Line 100 ptr HI
        STA $1500,X
        INX
        LDA #$0F
        LDY #99             ; Other 99 lines for a total of 200.
exp_1:  STA $1500,X
        INX
        DEY
        BNE exp_1
done_expansion:
        LDA #$41            ; ANTIC Jump
        STA $1500,X
        LDA #$00            ; Set jump address.
        STA $1501,X
        LDA #$15
        STA $1502,X

        ; Set new display list for text (lo-res).
        LDA #$00            ; Low byte of display list address
        STA $D402           ; Store in DLISTL
        LDA #$14            ; High byte of display list address
        STA $D403           ; Store in DLISTH
        ; Enable ANTIC DMA
        LDA #$22            ; Enable display list DMA (and set normal playfield mode).
        STA $D400           ; DMACTL

        ; Enable NMIs
        LDA #$C0
        STA $D40E           ; NMIEN
        
        ; Enable IRQs
        ;LDA #$F7
        ;STA $D20E           ; IRQEN
        
        ; Set border color.
        LDA #$00
        STA COL_BORDER

.endif ; BUILD_ATARI


        ; Initialize unsigned mul tables. This is required also by "print_str", so do it now.
        JSR mulu_init 

.if !BUILD_BEEB
        ; Print intro message (first message printed must start with $93=CLS).
        LDA #<str_intro
        STA str_ptr
        LDA #>str_intro
        STA str_ptr+1
        JSR print_str
.endif        
      
        ; Check machine.

.if BUILD_C64
        ; Check for Kawari, trying to enable extended features.
        LDA #86 ; 'V'
        STA $D03F
        LDA #73 ; 'I'
        STA $D03F
        LDA #67 ; 'C'
        STA $D03F
        LDA #50 ; '2'
        STA $D03F
        LDA $D03F
        AND #$80
        BNE no_kawari
        LDA #MODE_KAWARI
        STA mode
        ; Print "Kawari found" message.
        LDA #<str_kawari
        STA str_ptr
        LDA #>str_kawari
        STA str_ptr+1
        JSR print_str
        ; Zero out IDX regs
        LDA #$00
        STA $D035            ; VIDEO_MEM_1_IDX
        STA $D036            ; VIDEO_MEM_2_IDX
        LDA #$21             ; Enable extra regs overlay, and auto-increment mem port 1 on write.
                             ; NOTE: Auto-increment does not seem to work.
        STA $D03F            ; VIDEO_MEM_FLAGS
        LDA #$00
        STA $D03A            ; VIDEO_MEM_1_HI
        ; Kawari allows us to define nice palette gradients.
        ;
        ; Setup DVI/VGA 
        ; DVI/VGA palette is a 16*4 array of RGBx (6 bits each, 4th byte unused).
        LDY #$40             ; Point to first PAL_RGB register.
        LDX #0
-       STY $D039            ; VIDEO_MEM_1_LO
        INY
        LDA kawari_palette_RGB,X ; This is 8 bit, so we convert it to 6 bits.
        LSR
        LSR
        STA $D03B            ; Write value to video mem 1 val (and auto-increment).
        INX
        CPX #16*4
        BNE -
        ;
        ; Setup LPA color palette, 16 bytes LUMA, 16 bytes PHASE, 16 bytes AMPL.
        LDY #$A0             ; Point to first LPA register.
        LDX #0
-       STY $D039            ; VIDEO_MEM_1_LO (update ptr as auto-increment does not seem to be working).
        INY
        LDA kawari_palette_LPA,X  ; No conversion needed.
        STA $D03B            ; Write value to video mem 1 val (and auto-increment).
        INX
        CPX #16*3
        BNE -        
        ; Set color of first 256 bytes of CRAM to make intro text visible after redefining palette.
        LDX #$00
        LDA #$09
-       STA COL_RAM,X
        INX
        BNE -

no_kawari:

        ; Just map RAM and I/O. We don't need no Kernal.
        LDA $01
        AND #$F8
        ORA #$05
        STA $01
.endif ;BUILD_C64
        

.if BUILD_C128
        ; Detect C128.
        LDA $D030
        AND #$02           ; Check VIC-IIe test bit (should be 0 on a C128).
        BNE no_C128        ; This is not a C128.
        
        ; Check for 80-col mode.
        LDA $D7
        AND #$80
        BEQ no_C128_80col
        LDA $D030          ; Enable 2 MHz mode.
        ORA #$01
        STA $D030
        LDA #MODE_VDC
        STA mode

        ; Setup 80x50 chars mode (each char is 8x4).
        ; NOTE: This has not been tested on a CRT (only in VICE).
        LDX #$09            ; VDC[CTV]: Rasterlines per char row. (Default 7).
        LDA #$03
        JSR vdc_write

        LDX #$0C            ; VDC[DSh]: Display start HI. (Default $00).
;JSR vdc_read
;JSR print_A_hex 
        LDA #$00
        JSR vdc_write
        LDX #$0D            ; VDC[DSl]: Display start LO. (Default $00).
        LDA #$00
        JSR vdc_write

        LDX #$17            ; VDC[DSl]: Char rasterlines - 1. (Default 8).
        LDA #$03
        JSR vdc_write

        LDX #$04           ; VDC[VT]: Vertical Total (char rows - 1). (Default PAL=38, NTSC=32).
        LDA #63            ; NTSC
        ;LDA #66            ; PAL?
        JSR vdc_write
        
        LDX #$05           ; VDC[VA]: Vertical Adjust (rasterlines) (Default 0).
        LDA #0
        JSR vdc_write
        
        LDX #$06           ; VDC[VD]: Vertical Displayed (visible char rows). (Default 25).
        LDA #50
        JSR vdc_write
        
        LDX #$07           ; VDC[VP]: Vertical Sync Position (Default PAL=32, NTSC=29).
        LDA #55            ; NTSC
        ;LDA #52            ; PAL?
        JSR vdc_write
        
        LDX #$14           ; VDC[AAh]: Attribute Address HI (Default=$08).
        LDA #$10
        JSR vdc_write
        LDX #$15           ; VDC[AAl]: Attribute Address LO (Default=$00).
        LDA #$00
        JSR vdc_write
no_C128_80col:
.endif ;BUILD_C128
no_C128:
       
        ; C64 and C128 initialization.
.if BUILD_C64 | BUILD_C128        
        ; Setup CIA1
        LDA #$FF
        STA $DC02 ; DDRA all R/W
        STA $DC03 ; DDRB all R/W
        
        ; Setup VIC-II
        LDA #$00
        STA COL_BORDER
        LDA #$00
        STA COL_BGND
.elif BUILD_TED
        LDA #$FF
        STA $FD30         ; 6529B Keyboard scan mask.
        ; Init TED Time #1 for user input timing.
        LDA #$00
        STA $FF00
        LDA #$04
        STA $FF01
        LDA #$08          ; Set video matrix at $800
        STA $FF14
        ; Unmap ROM out of the way.
        LDA #$00   ; Dummy value.
        STA $FF3F  ; Any write to this register unmaps ROM.

        ; Setup TED
        LDA #$00
        STA COL_BORDER
        LDA #$00
        STA COL_BGND
.endif
        
        ; Print wait msg.
        LDA #<str_wait
        STA str_ptr
        LDA #>str_wait
        STA str_ptr+1
        JSR print_str
        
        ; Only build squares table if we have at least 64 KB RAM.
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_ATARI
        ; Initialize Q5.9 squares table.
        JSR init_squares_q5_9
.endif

.if BUILD_ATARI
        ; Enable special GTIA mode (16 luma).
        LDA $D01B
        AND #$3F
        ORA #%01000000       ; Set the GTIA mode bits (7-6) for 1 color / 16 luma.
        STA $D01B            ; PRIOR
        
        ; Customize the first 16 characters to display solid luma patterns (2x8 pixels).
        LDA #%00000000
        LDX #$00
nxt_char_pattern:
        LDY #8               ; Fill all 8 char lines.
-       STA $1000,X
        INX
        DEY
        BNE -
        CLC
        ADC #%00010001       ; Go to next pattern (2 pixels per char line).
        BCC nxt_char_pattern
.endif

        ; Enter main Mandelbrot code.        
        JSR Mandelbrot
exit:
        ; We'll never get back to here.
        ; We can't return to BASIC, as we have messed up the system quite good.
        ; Besides, there is no QUIT button ;-)
        RTS 


;==============================================================
; Strings
str_intro:
        .byte $93 ;Clear screen.      
        ;.text $0D, "..!? 012349 ABC abcdefghijklmnop" ; Used for testing.
        .text $0D
        .text "ddt's fixed-point mandelbrot", $0D
        .text "version 2024-12-02", $0D
        ;.text "https://github.com/0x444454/mandelbr8", $0D
        .byte $00

.if BUILD_C64
str_kawari:
        .text "found kawari.", $0D
        .byte $00
.endif        

str_wait:
        .text "please wait...", $0D
        .byte $00

;==============================================================
; Atari stuff
           
.if BUILD_ATARI
; Atari display list for text mode (lo-res).
atari_DL_text:
    .fill   3, $70              ; Blank 8 lines.
    .byte      $42              ; ANTIC mode 2 (40x25), 1 line + set ptr.
    .byte      <SCR_RAM         ; SCR_RAM LO
    .byte      >SCR_RAM         ; SCR_RAM HI
    .fill  24, $02              ; ANTIC mode 2 (40x25), 24 more lines.
    .byte      $41              ; Jump and wait for vertical blank.
    .byte      <atari_DL_text   ; Jump address LO
    .byte      >atari_DL_text   ; Jump address HI

; Atari display list for text mode (hi-res).
atari_DL_bitmap:
    .fill   3, $70              ; Blank 8 lines.
    .byte      $4F              ; ANTIC mode F.1 (80x200), 1 line + set ptr.
    .byte      <BITMAP_START    ; bitmap LO
    .byte      >BITMAP_START    ; bitmap HI
    .fill   1, $0F              ; ANTIC mode F.1 (80x200), 199 more lines.
                                ; NOTE: Just one line in this template. We'll expand it during copy.
    .byte      $41              ; Jump and wait for vertical blank.
    .byte      <atari_DL_bitmap ; Jump address LO
    .byte      >atari_DL_bitmap ; Jump address HI

; Atari NMI routine.
atari_NMI:
        INC frame_couter
        RTI

frame_couter: .byte 0

; Atari IRQ routine.
atari_IRQ:
        INC $401
        RTI
.endif
           
;------------- system & configs -------------

.if BUILD_C64
  SCR_RAM      = $0400
  COL_RAM      = $D800
  COL_BORDER   = $D020
  COL_BGND     = $D021
  LORES_W      = 40
  LORES_H      = 25
  HIRES_W      = 160
  HIRES_H      = 200
  HIRES_TILE_W = 4
  HIRES_TILE_H = 8  
  BITMAP_START = $2000
.elif BUILD_C128
  SCR_RAM      = $0400
  COL_RAM      = $D800
  COL_BORDER   = $D020
  COL_BGND     = $D021
  LORES_W      = 40
  LORES_H      = 25
  HIRES_W      = 160
  HIRES_H      = 200
  HIRES_TILE_W = 4
  HIRES_TILE_H = 8  
  BITMAP_START = $2000
.elif BUILD_TED
  SCR_RAM      = $0C00 ; Video matrix
  COL_RAM      = $0800 ; Attributes matrix
  COL_BORDER   = $FF19
  COL_BGND     = $FF15 ; Background register #0
  LORES_W      = 40
  LORES_H      = 25
  HIRES_W      = 160
  HIRES_H      = 200
  HIRES_TILE_W = 4
  HIRES_TILE_H = 8  
  BITMAP_START = $2000
.elif BUILD_PET
  SCR_RAM      = $8000 ; Video matrix.
  COL_RAM      = 0     ; No color RAM. Unfortunately, TASS64 has no .ifdef.
  COL_BORDER   = $FFFF ; Dummy.
  COL_BGND     = $FFFF ; Dummy.
  LORES_W      = 40
  LORES_H      = 25
  HIRES_W      = 160
  HIRES_H      = 200
  HIRES_TILE_W = 4
  HIRES_TILE_H = 8
  BITMAP_START = $2000
.elif BUILD_ATARI
  SCR_RAM      = $0400 ; Video matrix.
  COL_RAM      = $0400 ; ?
  COL_BORDER   = $D01A ; COLBK (border is same as background).
  COL_BGND     = $D01A ; ?
  LORES_W      = 40
  LORES_H      = 25
  HIRES_W      = 80
  HIRES_H      = 200
  HIRES_TILE_W = 2
  HIRES_TILE_H = 8
  BITMAP_START = $2060 ; Bitmap starts at $2060 to allow hitting $3000 exactly in the middle of the screen (ANTIC 4KB limitation). 
.elif BUILD_BEEB
  SCR_RAM      = $3000 ; Video matrix (this is actually a MODE 2: bitmap).
  COL_RAM      = $3000 ; No color RAM. Unfortunately, TASS64 has no .ifdef.
  COL_BORDER   = $FFFF ; Dummy.
  COL_BGND     = $FFFF ; Dummy.
  LORES_W      = 40
  LORES_H      = 32
  HIRES_W      = 160
  HIRES_H      = 255
  HIRES_TILE_W = 4
  HIRES_TILE_H = 8
  BITMAP_START = $3000
.endif


; The following is a bitmask, for faster checks.
MODE_VIC2     =   $01
MODE_KAWARI   =   $02
MODE_VDC      =   $04
MODE_BEEB     =   $08

mode:    .byte MODE_VIC2 ; Default to VIC-II mode.
res:     .byte 0         ; 0 if text/lo-res, 1 if hi-res. Default to text/lo-res.

.if BUILD_C64
kawari_palette_RGB:
    .byte   0,   0,   0,  0
    .byte  10,   7,  40,  0
    .byte   9,   1,  60,  0
    .byte   4,   4,  73,  0
    .byte   0,   7, 100,  0
    .byte  12,  44, 138,  0
    .byte  24,  82, 177,  0
    .byte  57, 125, 209,  0
    .byte 134, 181, 229,  0
    .byte 211, 236, 248,  0
    .byte 241, 233, 191,  0
    .byte 248, 201,  95,  0
    .byte 255, 170,   0,  0
    .byte 204, 128,   0,  0
    .byte 153,  87,   0,  0
    .byte 106,  52,   3,  0
kawari_palette_LPA:
    .byte $19, $02, $00, $20, $27, $2e, $30, $33, $36, $38, $37, $35, $33, $30, $2c, $28 ; Luma  (6 bits).
    .byte $00, $00, $00, $0a, $05, $f0, $e0, $d0, $ca, $70, $66, $66, $63, $61, $69, $6a ; Phase (8 bits).
    .byte  $0,  $8,  $c,  $f,  $f,  $f,  $f,  $f,  $6,  $4,  $b,  $f,  $e,  $f,  $f,  $f ; Amp   (4 bits).
.endif

;------------- print routine (no Kernal was harmed in the making of this routine) -------------
; Print zero-terminated string pointed to by (str_ptr), max 255 chars.
; This routine trashes registers and changes (str_ptr).
print_str:
pr_ch:
        LDY #$00           ; Clear index.
        LDA (str_ptr), Y   ; Load the next char from the message.
        BEQ pr_end         ; If character is 0 (end of string), jump to end
        CMP #$0D           ; <CR>
        BNE +
        LDA #0
        STA cursor_x
        INC cursor_y
        JSR calc_scr_addr
        JMP done_char
+       
        CMP #$93           ; <CLS>
        BNE +
        ; Clear text screen (and some more :-) with all reverse spaces.
        LDX #$00
-       
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET        
        LDA #$20           ; Commodore Space
.else
        LDA #$00           ; Atari Space
.endif        
        STA SCR_RAM,X
        STA SCR_RAM+$100,X
        STA SCR_RAM+$200,X
        STA SCR_RAM+$300,X
.if COL_RAM && !BUILD_ATARI
        LDA #$71           ; Upper nibble is only used on TED machines for luma.
        STA COL_RAM,X
        STA COL_RAM+$100,X
        STA COL_RAM+$200,X
        STA COL_RAM+$300,X
.endif ; def COL_RAM
        INX
        BNE -
        LDA #$00
        STA cursor_x
        STA cursor_y
        JSR calc_scr_addr
        JMP done_char
+       
        ; ASCII char. Convert to screen code.
        CLC
        CMP #32
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET        
        BPL +
        ADC #128 ; Commodore
.else
        BCC +
        ; ASCII lower than 32.
        SBC #32 ; Atari
.endif        
        JMP done_conv
+       CMP #64
        BMI done_conv
        CMP #97
        BPL +
        SEC
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET        
        SBC #64
.else
        SBC #32 ; ATARI
.endif        
        JMP done_conv
+       CMP #128
        BPL done_conv
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET          
        SEC
        SBC #32
.endif        
        JMP done_conv
        LDA #46        ; Unkown. Use ".".
done_conv:
        ; Normal screen code.
        LDY #$00
        STA (scr_ptr),Y
done_char_move_pos:
        INC cursor_x
        INC scr_ptr
        BNE +
        INC scr_ptr+1
+
done_char:    
        INC str_ptr         ; Increase char*.
        BNE +
        INC str_ptr+1
+       JMP pr_ch
pr_end:
        RTS

calc_scr_addr:
        ; Default to 40 columns. Mul by 40.
        LDA #40
        STA x0
        LDA mode
        AND #MODE_VDC
        BEQ +
        ASL x0             ; Actually, it is 80 columns.
+       LDA cursor_y
        STA y0
        LDA #$00           ; 256x256 chars will be enough for everyone...
        STA x1
        STA y1
        JSR multiply_16bit_unsigned
        ; Y-offset is in [z0..z1].
        LDA z0
        CLC
        ADC cursor_x
        STA scr_ptr
        LDA #$00
        ADC z1
        ADC #>SCR_RAM
        STA scr_ptr+1
        RTS

cursor_x:   .byte 0
cursor_y:   .byte 0


;------------- clear screen -------------
clear_screen:
.if BUILD_BEEB
        ; Just clear the entire bitmap: $3000..$7FFF
        LDA #$00
        STA bmp_ptr
        LDA #$30
        STA bmp_ptr+1
        LDY #0
nxt_page:
        LDA #$00
-       STA (bmp_ptr),Y
        INY
        BNE -
        INC bmp_ptr+1
        LDA bmp_ptr+1
        CMP #$80
        BNE nxt_page
        RTS
.endif
        LDA mode
        AND #MODE_VIC2 | MODE_KAWARI
        BEQ +
        LDX #$00
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET        
        LDA #$A0 ; Commodore: Use reverse spaces.
.else
        LDA #$20 ; Atari: Use spaces.
.endif
-       STA SCR_RAM,X
        STA SCR_RAM+$100,X
        STA SCR_RAM+$200,X
        STA SCR_RAM+$300,X
        INX
        BNE - ; Print till zero term or max 256 chars.
        ; Clear attribs (and some more) to white.
        LDX #$00
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET        
        LDA #$71 ; Commodore: White (upper nibble is only used by TED for luma).
.else
        LDA #$00 ; Atari: Black.
.endif
.if COL_RAM        
-       STA COL_RAM,X
        STA COL_RAM+$100,X
        STA COL_RAM+$200,X
        STA COL_RAM+$300,X
        INX
        BNE - ; Print till zero term or max 256 chars.
.endif        
        RTS
+   

.if BUILD_C128
        LDA mode
        AND #MODE_VDC
        BEQ +
        JSR vdc_cls
        RTS
+   
.endif
   
        RTS
    
.if BUILD_C128    
;------------- VDC reg read -------------
; Inputs
;   X: Register number
; Outputs:
;   A: Value

vdc_read:
        STX $D600        ; VDC register number.
-       BIT $D600
        BPL -
        LDA $D601        ; VDC register data.
        RTS

;------------- VDC reg write -------------
; Inputs
;   X: Register number
;   A: Value

vdc_write:
        STX $D600        ; VDC register number.
-       BIT $D600
        BPL -
        STA $D601        ; VDC register data.
        RTS

;------------- VDC clear screen -------------
vdc_cls:
        LDX #$12          ; VDC mem addr HI
        LDA #$00
        JSR vdc_write
        INX               ; VDC mem addr LO
        JSR vdc_write
        ; Repeat for number of chars to set.
        LDA #$10          ; Outer counter (16 loops)
        STA $FF
        LDY #$00          ; Inner counter (256 loops).
        LDX #$1F          ; VDC data register number.
        LDA #$A0          ; Char code to write.
-       JSR vdc_write
        DEY
        BNE -
        DEC $FF
        BNE -
        
        JSR check_userinput
        
        LDX #$12          ; VDC mem addr HI
        LDA #$10
        JSR vdc_write
        INX               ; VDC mem addr LO
        LDA #$00
        JSR vdc_write
        ; Repeat for number of attribs to set.
        LDA #$10          ; Outer counter (16 loops)
        STA $FF
        LDY #$00          ; Inner counter (256 loops).
        LDX #$1F          ; VDC data register number.
        LDA #$8F          ; Attribute to write.
-       JSR vdc_write
        DEY
        BNE -
        DEC $FF
        BNE -
    
        RTS               ; Return
.endif ; BUILD_C128

;------------- Switch lo/hi res -------------
; Note: Not all platforms support switching.
; Output: Z = 0 if mode switch succeeded (use BNE to branch on success).
; Clobbered: A, X
switch_res:
        LDA res
        TAX             ; Save to revert on error.
        EOR #$01        ; Toggle resolution.
        STA res
        JSR apply_mode
        BNE +           ; Success.
        STX res         ; ERROR: Revert res.
+       RTS

;------------- Apply mode -------------
; Configure machine based on current mode and resolution setting.
; Output: Z = 0 if mode switch succeeded (use BNE to branch on success).
; Clobbered: A, X
apply_mode:
        LDA mode
.if BUILD_C64 | BUILD_C128
        CMP #MODE_VDC
        BNE chk_VIC2_res
        ; VDC
        LDA res
        CMP #$01        ; Hi-res not yet supported on VDC.
        RTS
chk_VIC2_res:
        ;CMP #MODE_VIC2
        ;BNE chk_KAWARI_res ; Kawari hi-res not yet supported.
        ; VIC2
        LDA res
        BNE +
        ; VIC2 lo-res (text)
        LDA $D011       ; Setup CR1
        AND #$9F        ; Clear ECM and BMM
        STA $D011
        LDA $D016       ; Setup CR2
        AND #$EF        ; Clear MCM
        STA $D016
        LDA #$14        ; Screen RAM at $400
        STA $D018
        LDA #1          ; Return OK
        JMP end_applymode
+       ; VIC2 hi-res, multicolor bitmap: 3 diff colors per 4x8 cell + background color.
        LDA $D011       ; Setup CR1
        AND #$9F        ; Clear ECM and BMM
        ORA #$20        ; Set BMM.
        STA $D011
        LDA $D016       ; Setup CR2
        ORA #$10        ; Set MCM
        STA $D016
        LDA #$18        ; Screen RAM at $0400 (high-low color), bitmap at [$2000..$4000].
        STA $D018
        JSR clear_bitmap
        LDA #1          ; Return OK
        JMP end_applymode
chk_KAWARI_res:
        ; KAWARI_GFX
        LDA res
        ;CMP #$01        ; Hi-res not yet supported on Kawari.
        JMP end_applymode
+
.elif BUILD_TED
        LDA res
        BNE +
        ; TED lo-res (text)
        LDA $FF12
        ORA #$04        ; Point to ROM (set bit 2).
        STA $FF12
        ; Clear multicolor mode.
        LDA $FF07
        AND #$EF
        STA $FF07
        ; Disable bitmap mode.
        LDA $FF06
        AND #$5F        ; Set also TED test bit (7) to 0.
        STA $FF06
        LDA #1          ; Return OK
        JMP end_applymode
+       ; TED hi-res (bitmap multicolor: 2 diff colors per 4x8 cell + 2 background colors (#0, #1).
        JSR clear_bitmap
        ; Copy colors for multicolor %10 to video matrix and set luma to 6 for all others.
        LDX #$00
-       LDA COL_RAM,X
        STA SCR_RAM,X
        LDA #$66
        STA COL_RAM,X
        
        LDA COL_RAM+$100,X
        STA SCR_RAM+$100,X
        LDA #$66
        STA COL_RAM+$100,X
        
        LDA COL_RAM+$200,X
        STA SCR_RAM+$200,X
        LDA #$66
        STA COL_RAM+$200,X
        
        LDA COL_RAM+$300,X
        STA SCR_RAM+$300,X
        LDA #$66
        STA COL_RAM+$300,X
        
        INX
        BNE -
        ; Set Background #1 to light gray (less visible artifact for pastel colors).
        LDA #$61
        STA $FF16
        ; Set bmp base to $2000.
        LDA $FF12
        AND #$C3        ; Also, point to RAM (clear bit 2).
        ORA #$08
        STA $FF12
        ; Set multicolor mode.
        LDA $FF07
        ORA #$10
        STA $FF07
        ; Enable bitmap mode.
        LDA $FF06
        ORA #$20
        STA $FF06
        LDA #1          ; Return OK
        JMP end_applymode
.elif BUILD_ATARI
        LDA #$14            ; High byte of display list address (default to lo-res).
        LDX res
        BNE +
        JMP switch_display_list
+       ; ATARI hi-res
        ; Copy lo-res image to hi-res.
        LDA #40
        STA num_tiles_w
        LDA #25
        STA num_tiles_h
        LDA #<SCR_RAM
        STA scr_ptr
        LDA #>SCR_RAM
        STA scr_ptr+1
        LDA #<BITMAP_START
        STA bmp_ptr
        LDA #>BITMAP_START
        STA bmp_ptr+1
        LDY #0
        LDA #0
        STA tiley
cpy_nxt_lr_tile_row:
        LDA #0
        STA tilex
cpy_nxt_lr_tile:        
        LDA (scr_ptr),Y
        ROL
        ROL
        ROL
        ROL
        AND #$F0
        STA (bmp_ptr),Y
        LDA (scr_ptr),Y
        AND #$0F
        ORA (bmp_ptr),Y
        STA (bmp_ptr),Y
        STA tmp_bmp_line   ; Save value to store in this tile.
        LDA bmp_ptr
        PHA
        LDA bmp_ptr+1
        PHA
        LDX #7              ; Write value to the remaining 7 tile lines.
cpy_t_line:
        LDA bmp_ptr
        CLC
        ADC num_tiles_w     ; This is a linear buffer, skip down one line.
        STA bmp_ptr
        LDA bmp_ptr+1
        ADC #0
        STA bmp_ptr+1
        LDA tmp_bmp_line
        STA (bmp_ptr),Y
        DEX
        BNE cpy_t_line
        PLA
        STA bmp_ptr+1
        PLA
        STA bmp_ptr
        INC scr_ptr
        BNE +
        INC scr_ptr+1
+       TXA
        PHA
        INC tilex
        JSR bmp_to_next_tile
        PLA
        TAX
        LDA tilex
        CMP num_tiles_w
        BNE cpy_nxt_lr_tile
        ; Copied a tile row.
        INC bmp_ptr
        BNE +
        INC bmp_ptr+1
+       INC tiley
        LDA tiley
        CMP num_tiles_h
        BNE cpy_nxt_lr_tile_row
        ; Done copying. Reset tilex and tiley.
        LDA #$00
        STA tilex
        STA tiley
        ; Set hi-res display list.
        LDA #$15
switch_display_list:
        ; Disable ANTIC DMA
        LDX #$00            ; Disable display list DMA.
        STX $D400           ; DMACTL
        ; Switch display list.
        STA $D403           ; Store in DLISTH
        ; Enable ANTIC DMA
        LDA #$22            ; Disable display list DMA.
        STA $D400           ; DMACTL
        LDA #1              ; Return OK
        JMP end_applymode
.elif BUILD_BEEB
        ; BBC Micro.
        ; Not much to do here, as we run the Beeb always in bitmap mode.
        ; Reset bitmap pointers.
        LDA #$00
        STA bmp_ptr
        LDA #$30
        STA bmp_ptr+1
        ; Return OK
        LDA #1
        JMP end_applymode
.else
        ; Unhandled mode.
        LDA #0          ; Return ERROR
.endif
end_applymode:
;ee: LDA res
;    BNE ee
        RTS

tmp_bmp_line: .byte 0


;------------- Clear bitmap -------------
; Clear the bitmap ($2000 to $3FFF). That works for Atari too (bitmap starts at $2060).
; Clobbered: A, X
clear_bitmap:
        LDA #$20
        STA str_ptr+1
        LDA #$00
        STA str_ptr
        LDX #$00
-       
.if BUILD_C64 | BUILD_C128
        LDA #$FF        ; Default to multicolor 3 (taken from Color RAM which should be already set).
.elif BUILD_TED
        LDA #$AA        ; Default color to video matrix [3−0], luma to attribute matrix [6−4].
.endif        
        STA (str_ptr,X)
        INC str_ptr
        BNE -
        INC str_ptr+1
        LDA str_ptr+1
        CMP #$40
        BNE -
        RTS

;------------- Clear iters histogram -------------
clear_histogram:
        LDA #$00
        LDX #16       ; Colors histogram is 16 bytes.
-       STA b_hist,X
        DEX
        BNE -
        RTS

;------------- Build histogram -------------
; Build the color histogram from the content of iterations buffer.
; Only the first 4 bits of iter are used (mod16), so the histogram takes 16 bytes.
build_histogram:
        JSR clear_histogram
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        LDY #0
-       LDA (buf_it_ptr),Y
        AND #$0F          ; Mod16.
        TAX
        INC b_hist,X
        INY
        CPY buf_tile_size
        BNE -
        RTS

;------------- Scan histogram -------------
; Find the 4 most used colors (mod16), and store them sorted in h_top4 (higher to lower).
; NOTE: Black (color 0) is not counted, as it is the fixed background color (and always available).
; NOTE: This is probably overkill, as we only need 3 colors + black (fixed background).
scan_histogram:
        ; Clear results.
        LDA #$00
        ; Clear top counts.
        STA h_top4_cnt
        STA h_top4_cnt+1
        STA h_top4_cnt+2
        STA h_top4_cnt+3
        ; Y is the result index (0 to 4).
        LDY #0
find_next:
        ; Scan all 16 entries.
        LDA #0
        STA h_top4_cnt,Y       ; Highest count found.
        STA h_top4_idx,Y       ; Index of highest found.
        LDX #15                ; Start with color 15 (down to 1).
-       LDA b_hist,X           ; Fetch color count.
        CMP h_top4_cnt,Y       ; Is this color more used than current most used ?
        BCC +                  ; BLT.
        ; Higher or equal found.
        STA h_top4_cnt,Y       ; Update highest count.
        STX h_top4_idx,Y       ; Update index of highest count.
+       DEX
        BNE -                  ; Stop at color 1, avoiding 0 (black) as the background color is always available.
        ; Found the index. Zero its histogram count, so we won't parse it again.
        LDA h_top4_idx,Y
        TAX
        LDA #$00
        STA b_hist,X 
        ; Find next.
        INY
        CPY #4
        BNE find_next
        RTS


;------------- Render tile (multicolor version) -------------
; Render a hi-res tile in multicolor mode.
; 
render_tile_multicolor:
.if BUILD_C64 | BUILD_C128
        ; Render a multicolor 4x8 tile based on the computed histogram.
        ; We need to choose between black or any of the other three most used colors in the tile.
        ; In multicolor bitmap, each pixel is encoded as two bits:
        ;   00 : Background color 0 ($D021)
        ;   01 : Color from bits 4-7  of c-data (Screen RAM hi-nibble).
        ;   10 : Color from bits 0-3  of c-data (Screen RAM lo-nibble).
        ;   11 : Color from bits 8-11 of c-data (Color RAM lo-nibble). 
        ;
        ; Set most used three colors in c-data (fourth color is always black).
        ;
        ; Color RAM (%11).
        LDA tile_num
        STA cram_ptr
        LDA tile_num+1
        CLC
        ADC #$D8
        STA cram_ptr+1
        LDA h_top4_idx+2
        LDY #0
        STA (cram_ptr),Y
        ; Screen RAM (%10 and %01).
        LDA tile_num+1
        CLC
        ADC #$04
        STA cram_ptr+1
        LDA h_top4_idx+1
        AND #$0F
        STA tmp_A
        LDA h_top4_idx+0
        ASL
        ASL
        ASL
        ASL
        ORA tmp_A
        STA (cram_ptr),Y
.elif BUILD_TED
        ; Point to video matrix.
        LDA tile_num
        STA cram_ptr
        LDA tile_num+1
        CLC
        ADC #>SCR_RAM       ; Video matrix.
        STA cram_ptr+1
        LDA h_top4_idx      ; Multicolor pattern %01 uses color from video matrix bits [7..4].
        ASL
        ASL
        ASL
        ASL
        LDY #0
        STA (cram_ptr),Y
        LDA h_top4_idx+1    ; Multicolor pattern %10 uses color from video matrix bits [3..0].
        AND #$0F
        ORA (cram_ptr),Y
        STA (cram_ptr),Y
.endif

.if BUILD_C64 | BUILD_C128 | BUILD_TED        
        ; bmp_ptr contains the current tile bitmap line (4 pixels, 2 bits each).
        ; buf_it_ptr is the per-pixel iterations buffer.
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        LDY #0           ; Current tile pixel [0..32].
nxt_tile_line_MC:
        LDA #$00         ; Preset line all black.
        TAX
        STA (bmp_ptr,X)
        LDX #3           ; Tile line pixel countdown [3..0]. We use this also as a shift counter.
nxt_tile_pix_MC:
        ; Fetch pixel iters and convert to color.
        LDA (buf_it_ptr),Y
        AND #$0F         ; Mod16.
        ; If black, pass-through as background color.
        BEQ found_conversion
        ; Search for exact match in histogram.
        CMP h_top4_idx   ; Compare with 1st most used.
        BNE +
        LDA #%01
        JMP found_conversion
+       CMP h_top4_idx+1 ; Compare with 2st most used.
        BNE +
        LDA #%10
        JMP found_conversion
+       CMP h_top4_idx+2 ; Compare with 3rd most used.
        BNE +
        LDA #%11
        JMP found_conversion
        ; No exact match. Just use lower 2 bits (but avoid black).
+       AND #%11
        BNE found_conversion
        LDA #%01         ; If %00 (black), then make it %01.
found_conversion:
        STX tmp_X        ; Save X.
        CPX #0
        BEQ done_adjust
-       ASL
        ASL
        DEX
        BNE -            ; Keep adjusting mask.
done_adjust:
        LDX #0
        ORA (bmp_ptr,X)
        STA (bmp_ptr,X)  ; Update tile bitmap line.
        INY              ; Point to next pixel.
        LDX tmp_X        ; Restore X (line pixel countdown).
        DEX
        BPL nxt_tile_pix_MC
        ; End of tile bitmap line.
        INC bmp_ptr      ; Inc bitmap ptr.
        BNE +
        INC bmp_ptr+1
+       CPY buf_tile_size
        BNE nxt_tile_line_MC
.endif
        RTS

tmp_A: .byte 0
tmp_X: .byte 0


.if BUILD_ATARI
;------------- Render tile (ATARI version) -------------
; Render a hi-res tile in ATARI mode F.1 (2x8 pixels per tile).
; Note that this is a linear format (40 bytes per line).
; 
render_tile_ATARI:
        ; Save bmp_ptr
        LDA bmp_ptr
        PHA
        LDA bmp_ptr+1
        PHA
        ; Simply use the 2x8 iters buffer as colors (Mod16).
        ; bmp_ptr contains the current tile bitmap line (2 pixels, 4 bits each).
        ; buf_it_ptr is the per-pixel iterations buffer.
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        LDY #0           ; Current tile pixel [0..16].
        LDX #0
nxt_tile_line_A:
        ; Each tile line contains two pixels.
        ; Fetch pixel iters and convert to color.
        LDA (buf_it_ptr),Y
        ROL
        ROL
        ROL
        ROL
        AND #$F0
        STA (bmp_ptr,X)
        INY
        LDA (buf_it_ptr),Y
        AND #$0F         ; Mask.
        ORA (bmp_ptr,X)
        STA (bmp_ptr,X)
        INY
        ; End of tile bitmap line (2 pixels).
        CLC
        LDA bmp_ptr
        ADC num_tiles_w   ; This is a linear buffer, skip down one line.
        STA bmp_ptr
        LDA bmp_ptr+1
        ADC #0
        STA bmp_ptr+1
        CPY #HIRES_TILE_W*HIRES_TILE_H
        BNE nxt_tile_line_A
        ; Retreive bmp_ptr.
        PLA
        STA bmp_ptr+1
        PLA
        STA bmp_ptr
        JSR bmp_to_next_tile
        RTS
.endif

.if BUILD_BEEB
;------------- Render tile (BBC Micro version) -------------
; Render a hi-res tile in Mode 2.
; We use a 4x8 tile format.
; The BBC Micro uses 8 colors per pixel, plus a useless flash attribute.
; Each byte encodes 2 horizontal pixels (fbgr and FBGR) in bit-interleaved format: fFbBgGrR.
; For each 4x8 cell, bytes are rendered on screen like this ("byte_offset=format"):
;   00=fFbBgGrR  08=fFbBgGrR
;   01=fFbBgGrR  09=fFbBgGrR
;   02=fFbBgGrR  0a=fFbBgGrR
;   03=fFbBgGrR  0b=fFbBgGrR
;   04=fFbBgGrR  0c=fFbBgGrR
;   05=fFbBgGrR  0d=fFbBgGrR
;   06=fFbBgGrR  0e=fFbBgGrR
;   07=fFbBgGrR  0f=fFbBgGrR
;
; Our algorithm renders two half-tiles (first 2x8 left, and then 2x8 right).
;
render_tile_BEEB:
        ; Simply use the 4x8 iters buffer as colors (Mod8).
        ; bmp_ptr contains the current half-tile bitmap byte (2 pixels, 4 bits each).
        ; buf_it_ptr is the per-pixel iterations buffer.
        
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        LDA #2
        STA halves_countdown     ; Tile halves countdown (2 halves).
        LDY #0                   ; Current tile pixel in iters buffer [0..32].
nxt_halftile_BEEB:      
        LDA #8
        STA line_countdown       ; Tile line countdown.
nxt_tile_byte_BEEB:
        ; Each bitmap byte contains 2 pixels (left and right).
        ; Alas, color bits are interleaved, so we use a table to interleave them.
        LDA (buf_it_ptr),Y          ; Get left pixel.
        AND #$07                    ; Only 8 colors available.
        INY
        TAX
        LDA mode2_solid_colors,X    ; Convert left-pixel to interleaved using table.
        AND #$AA
        STA tmp_conv
        LDA (buf_it_ptr),Y          ; Get right pixel.
        AND #$07                    ; Only 8 colors available.
        INY
        TAX
        LDA mode2_solid_colors,X    ; Convert right-pixel to interleaved using table.
        AND #$55
        ORA tmp_conv                ; Interleave left and right pixels.
        LDX #0
        STA (bmp_ptr,X)             ; Set bitmap byte.
        INC bmp_ptr
        BNE +
        INC bmp_ptr+1
+       INY                         ; Skip two pixels (we are rendering half-tile in this inner loop).
        INY
        DEC line_countdown
        BNE nxt_tile_byte_BEEB
        ; Half-tile done.
        DEC halves_countdown
        BEQ done_tile_BEEB
        ; Get back to right-tile pixels.
        TYA
        SEC
        SBC #30
        TAY
        BNE nxt_halftile_BEEB       ; NOTE: This is actually an unconditional relative branch.
done_tile_BEEB:

        RTS
.endif

halves_countdown:   .byte 0
line_countdown:     .byte 0
tmp_conv:           .byte 0    

    
;------------- Mandelbrot calculation -------------
.if BUILD_BEEB
        * = $1C00
.else
        * = $4000
.endif

Mandelbrot:
        ; Max iters
        LDA #16
        STA max_iter
    
        ; Default coordinates (fixed_point, *1024).
        start_ax = -2200
.if BUILD_BEEB
        ; Center vertically for 256 lines displays.
        start_ay =  1440
.else
        ; Center vertically for 200 lines displays.
        start_ay =  1248
.endif        
        
        LDA #<start_ax
        STA ax
        LDA #>start_ax
        STA ax+1
    
        LDA #<start_ay
        STA ay
        LDA #>start_ay
        STA ay+1
    
        ; Set default lo-res increments, depending on mode.
        LDA #0
        STA incx_lr+1
        STA incy_lr+1    
        LDA mode
        AND #MODE_VIC2 | MODE_KAWARI | MODE_BEEB
        BEQ +
        LDA #96             ; Must be a multiple of 8 to keep lo-res and hi-res aligned.
        STA incx_lr
        STA incy_lr
        JMP done_incs
+       LDA mode
        CMP #MODE_VDC
        BNE +
        LDA #50
        STA incx_lr
        STA incy_lr
        JMP done_incs
+       ; Unknown/unhandled mode.
done_incs:


first_pass:
        ; First pass is lo-res.
        LDA #$00
        STA COL_BORDER
        ; Configure incs for first pass depending on video mode.
        LDA #0
        STA res           ; lo-res
        STA buf_tile_size ; 0 means screen size or none (depending on video mode).

calc_image:
        LDA #0
        STA res
        STA tilex
        STA tiley
        ; Configure graphics subystem.
        JSR apply_mode
        JSR clear_screen

nxt_pass:
        ; Preset incs for lo-res.
        LDA incx_lr
        STA incx
        LDA incx_lr+1
        STA incx+1
        LDA incy_lr
        STA incy
        LDA incy_lr+1
        STA incy+1
        ; Handle mode-dependent params.
        LDA mode
        AND #MODE_VIC2 | MODE_KAWARI | MODE_BEEB ; Check for VIC2 and similar chips.
        BNE +
        JMP no_VIC2ish_setup
        ;------ VIC2
+       LDA res
        BNE hi_res
        ; Lo-res. One single (LORES_W x LORES_H) tile.
        LDA #1
        STA num_tiles_w
        STA num_tiles_h
        LDA #0
        STA tile_num
        STA tile_num+1
        STA screenw+1
        STA screenh+1
        LDA #LORES_W
        STA screenw
        STA tilew
        LDA #LORES_H
        STA screenh
        STA tileh
        ; In lo-res we write directly to Color RAM after calculating a pixel.
        LDA #<COL_RAM
        STA cram_ptr
        LDA #>COL_RAM
        STA cram_ptr+1  
        JMP first_tile

hi_res:
        ; Hi-res mode second-pass (4x8 multicolor tile).
        ; Set tile width and height.
        LDA #<BITMAP_START
        STA bmp_ptr
        LDA #>BITMAP_START
        STA bmp_ptr+1
        LDA #HIRES_W
        STA screenw
        LDA #HIRES_H
        STA screenh
        LDA #0
        STA screenw+1
        STA screenh+1
        LDA #LORES_W
        STA num_tiles_w
        LDA #LORES_H
        STA num_tiles_h
        LDA #HIRES_TILE_W
        STA tilew
        LDA #HIRES_TILE_H
        STA tileh
        LDA #HIRES_TILE_W*HIRES_TILE_H ; Pixels per tile.
        STA buf_tile_size
        ; Update incs for second-pass.
.if BUILD_ATARI        
        LDX #1        ; Rotate twice to divide lo-res incx by 2.
.else
        LDX #2        ; Rotate twice to divide lo-res incx by 4.
.endif
-       CLC
        ROR incx+1
        ROR incx
        DEX
        BNE -
        LDX #3        ; Rotate thrice to divide lo-res incy by 8.
-       CLC
        ROR incy+1
        ROR incy
        DEX
        BNE -
        LDA incy
        BNE +
        ; Zoomed-in too much, set minimum incs.
        LDA #1
        STA incy
        ASL
        STA incx
+    
        ; Sub|add half lo-res pixel to ax|ay, to use previously calculated lo-res iter as centroid iter.
        LDA incx_lr
        TAX
        CLC
        ROR incx_lr
        LDA ax
        SEC
        SBC incx_lr
        STA ax
        LDA ax+1
        SBC #0
        STA ax+1
        STX incx_lr
        LDA incy_lr
        CLC
        ROR
        CLC
        ADC ay
        STA ay
        LDA ay+1
        ADC #0
        STA ay+1
        JMP first_tile
no_VIC2ish_setup:

.if BUILD_C128
        LDA mode
        CMP #MODE_VDC
        BNE +
        ;------ VDC
        ; VDC lo-res (one single 80x50 tile).
        LDA #1
        STA num_tiles_w
        STA num_tiles_h
        LDA #0
        STA screenw+1
        STA screenh+1    
        LDA #80
        STA screenw
        STA tilew
        LDA #50
        STA screenh
        STA tileh
        ; Point to start of VDC attributes.
        LDX #$12          ; VDC mem addr HI
        LDA #$10
        JSR vdc_write
        INX               ; VDC mem addr LO
        LDA #$00
        JSR vdc_write
        JMP first_tile
+
.endif ; BUILD_C128

first_tile:
        LDA ax
        STA t_ax
        LDA ax+1
        STA t_ax+1
        LDA ay
        STA t_ay
        LDA ay+1
        STA t_ay+1

nxt_tile:       ; A tile is the entire screen if not using a tiled hi-res video mode.
        ; Check if we can skip this tile.
        LDA res
        BEQ no_skip         ; Can't skip in lo-res.
        JSR check_skippable ; Sets Z if skippable.
        BNE no_skip
        ; Skip tile.
        JSR bmp_to_next_tile   ; Update bmp_ptr to point to next tile.
        JMP go_to_next_tile    ; Skip this tile.

no_skip:
        LDA #0              ; Reset cur pixel pos in tile.
        STA pixelx
        STA pixely
    
        ; (cx, cy) is our current point to calculate.
        ; Start with upper left point of tile (t_ax, t_ay).
        LDA t_ax
        STA cx
        LDA t_ax+1
        STA cx+1
        
        LDA t_ay
        STA cy
        LDA t_ay+1
        STA cy+1
    
        ; Setup the iterations buffer for this tile.
        LDA res
        BNE +
        ; Use lo-res iterations buffer.
        LDA #<buf_iters_lr
        STA buf_it_ptr
        LDA #>buf_iters_lr
        STA buf_it_ptr+1
        JMP done_it_buf
+       ; Use hi-res iterations buffer.
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        JMP done_it_buf
done_it_buf:
    
; Calculate current point (cx, cy).
calc_point:
        LDA #0   ; Reset iteration counter.
        STA iter
        STA zx   ; Reset (zx, zy)
        STA zx+1
        STA zy
        STA zy+1

nxt_iter:
        JSR check_userinput
        BNE no_recalc    ; No recalculation needed.
        ; User input requires image recalc. 
        JMP first_pass
no_recalc:
        ;   zx = zx + cx
        CLC     
        LDA zx  
        ADC cx  
        STA zx  
        LDA zx+1
        ADC cx+1
        STA zx+1
    
        ;   zy = zy + cy
        CLC     
        LDA zy  
        ADC cy  
        STA zy  
        LDA zy+1
        ADC cy+1
        STA zy+1
    
        ; Kawari code for squares commented out.
        ; This should be more precise than our table, but is it really faster ?
        ;
        ; zx2 = zx * zx
        ;LDA mode
        ;AND #MODE_KAWARI | MODE_KAWARI
        ;BEQ zx2_sw
        ;; Use Kawari hardware muls.
        ;LDA zx
        ;STA $D030       ; OP_1_LO
        ;STA $D032       ; OP_2_LO
        ;LDA zx+1
        ;STA $D02F       ; OP_1_HI
        ;STA $D031       ; OP_2_HI
        ;LDA #2          ; Kawari S_MULT operator.
        ;STA $D033       ; This triggers the operation.
        ;; Result is immediately available.
        ;; Divide result by 1024 (i.e. divide RESULT_HL, RESULT_LH by 4).
        ;LDA $D030       ; RESULT_HL
        ;STA $D02F       ; OP1_HI
        ;LDA $D031       ; RESULT_LH
        ;STA $D030       ; OP1_LO
        ;LDA #$00
        ;STA $D031       ; OP2_HI
        ;LDA #$04
        ;STA $D032       ; OP2_LO
        ;LDA #1          ; Kawari U_DIV operator.
        ;STA $D033       ; This triggers the operation.
        ;; Result is immediately available.
        ;LDA $D032       ; RESULT_LL
        ;STA zx2
        ;LDA $D031       ; RESULT_LH
        ;STA zx2+1
        ;JMP zx2_done
    
zx2_sw:    
        LDA zx
        STA x0
        STA y0
        LDA zx+1
        STA x1
        STA y1
.if BUILD_PET | BUILD_BEEB        
        JSR multiply_Q6_10_signed ; [z1..z2] = zx*zx
.else
        JSR square_Q10_6
.endif        
        LDA z1
        STA zx2
        LDA z2
        STA zx2+1
zx2_done:
    
        ; zy2 = zy * zy
        LDA zy
        STA x0
        STA y0
        LDA zy+1
        STA x1
        STA y1
.if BUILD_PET | BUILD_BEEB         
        JSR multiply_Q6_10_signed ; [z1..z2] = zy*zy
.else        
        JSR square_Q10_6
.endif        
        LDA z1
        STA zy2
        LDA z2
        STA zy2+1

        ; Check for divergence (zx2 + zy2 > 4.000).
        ; Note: In Q6.10 the number 4.000 = $1000.
        CLC
        LDA zx2   ; Maybe not needed.
        ADC zy2   ; Maybe not needed.
        LDA zx2+1
        ADC zy2+1
        CMP #$10  ; Just check high byte.
        BCC +
        JMP found_color ; Early exit (not black if >= 4.000).
+    
        ; Before overwriting zx, set it for the next muls operation.
        LDA zx
        STA x0
        LDA zx+1
        STA x1
        
        ; zx = zx2 - zy2
        SEC
        LDA zx2
        SBC zy2
        STA zx
        LDA zx2+1
        SBC zy2+1
        STA zx+1
    

        ; zy = 2 * zx * zy
        LDA mode
        AND #MODE_KAWARI
        BEQ zxzy_sw
        ; Use Kawari hardware muls.
        LDA x0
        STA $D030       ; OP_1_LO
        LDA x1
        STA $D02F       ; OP_1_HI
        LDA y0
        STA $D032       ; OP_2_LO
        LDA y1
        STA $D031       ; OP_2_HI
        LDA #2          ; Kawari S_MULT operator.
        STA $D033       ; This triggers the operation.
        ; Result is immediately available.
        LDA $D031       ; RESULT_LH
        STA z1
        LDA $D030       ; RESULT_HL
        STA z2
        LDA $D02F       ; RESULT_HH
        STA z3
        JMP zxzy_done
    
zxzy_sw: 
        ; No need to setup zx [x0,x1] and zy [y0,y1]. They are already there.
        JSR multiply_16bit_signed ; [z0..z3] = zx*zy*1024

zxzy_done:    
        LDA z3
        CMP #$80     ; Set carry if result is negative.
        ROR z3
        ROR z2
        ROR z1       ; [z1..z2] = 2*zx*zy
        LDA z1
        STA zy
        LDA z2
        STA zy+1

        ; Inc iter and check max_iter.
        INC iter
        LDA iter
        CMP max_iter
        BEQ found_color_black ; Max iters reached.
        JMP nxt_iter

found_color_black:
        LDA #0              ; Max iters is always black
        STA iter
found_color:
        LDA iter
        LDY #0
        STA (buf_it_ptr),Y  ; Save iters to pixel iterations buffer.
        INC buf_it_ptr
        BNE +
        INC buf_it_ptr+1
+       ; If we are in hi-res mode, we do not set Color RAM here.
        LDA res
        BNE skip_ColorRAM
        ; Lo-res.
        LDA mode
        AND #MODE_VIC2 | MODE_KAWARI
        BEQ no_VIC2
        ; MODE_VIC2
        LDA iter
        AND #$0F
.if BUILD_TED
        ORA #$60          ; Set luma to 6 (pastel colors) to conceal TED multicolor limitations.
.endif ;BUILD_TED    
        LDY #0
        STA (cram_ptr),Y  ; Set color
        INC cram_ptr
        BNE +
        INC cram_ptr+1
+       JMP nxt_point
no_VIC2:
.if BUILD_C128
        LDA mode
        CMP #MODE_VDC
        BNE +
        ; MODE_VDC
        LDA iter
        AND #$0F          ; Max 16 colors on VDC (and remove all special attributes).
        LDX #$1F          ; VDC data register (we already point to the next attribute byte).
        JSR vdc_write
        JMP nxt_point
+
.endif
.if BUILD_BEEB
        ; MODE_BEEB
        ; We don't have Color RAM, so we just fake it using bitmap.
        LDA iter
        AND #$07          ; Mode-2 is 8 colors.
        TAX
        LDA mode2_solid_colors,X    ; Use table to convert to solid color pattern (2 pixels of 4 bits each).
        LDY #0
        LDX #16           ; 2 pixels per byte. We need to set 4x8 = 32 pixels, hence 16 bytes.
-       STA (cram_ptr),Y  ; Set 2 pixels.
        INC cram_ptr
        BNE +
        INC cram_ptr+1
+       DEX
        BNE -
        JMP nxt_point
.endif

skip_ColorRAM:

; Go to nxt point.
nxt_point:
        ; cx = cx + incx
        CLC
        LDA cx
        ADC incx
        STA cx
        LDA cx+1
        ADC incx+1
        STA cx+1
        
        ; Point to next pixel and check end of rows.
        INC pixelx
        LDA pixelx
        CMP tilew       ; WARNING: This currently requires tile width < 256.
        BEQ nxt_row     ; End of tile row.
        JMP calc_point

        ; Jump to next pixel row in tile.
nxt_row:
        ; cy = cy + incy
        SEC
        LDA cy
        SBC incy
        STA cy
        LDA cy+1
        SBC incy+1
        STA cy+1
        
        ; cx = t_ax
        LDA t_ax
        STA cx
        LDA t_ax+1
        STA cx+1
    
        LDA #0
        STA pixelx
        INC pixely
        LDA pixely
        CMP tileh      ; WARNING: This currently requires tile height < 256.
        BEQ end_tile
        JMP calc_point

end_tile:
        ; End of tile.
        ; Check if we need to switch to hi-res mode.
        LDA res
        BEQ switch_to_hires
    
        ; We have completed a hi-res tile, render it.
.if BUILD_C64 | BUILD_C128 | BUILD_TED | BUILD_PET
        JSR build_histogram
        JSR scan_histogram
        JSR render_tile_multicolor
.elif BUILD_ATARI
        JSR render_tile_ATARI
.elif BUILD_BEEB
        JSR render_tile_BEEB
.endif        

go_to_next_tile:
        ; Calc next tile.
        INC tile_num
        BNE +
        INC tile_num+1
+       INC tilex
        LDA tilex
        CMP num_tiles_w
        BNE nxt_tile_in_row
        ; End of tile row, advance to next row.
.if BUILD_ATARI
        JSR bmp_to_next_tile    ; Needed on Atari to update linear bitmap pointer.
.endif        
        INC tiley
        LDA tiley
        CMP num_tiles_h
        BEQ no_more_passes      ; Hi-res pass done. Image completed.
        LDA #0
        STA tilex
        LDA ax
        STA t_ax
        LDA ax+1
        STA t_ax+1
        LDX #HIRES_TILE_H       ; Sub HIRES_TILE_H*incy to t_ay.
-       SEC
        LDA t_ay
        SBC incy 
        STA t_ay
        LDA t_ay+1
        SBC incy+1
        STA t_ay+1
        DEX
        BNE -
        JMP done_tile_advance
nxt_tile_in_row:
        ; Advance to next tile in row.
        LDX #HIRES_TILE_W         ; Add HIRES_TILE_W*incx to t_ax.
-       CLC
        LDA t_ax
        ADC incx 
        STA t_ax
        LDA t_ax+1
        ADC incx+1
        STA t_ax+1
        DEX
        BNE -
done_tile_advance:
    
        JMP nxt_tile

;----------------- Switch to hires -----------------
; After lo-res pass, switch to hi-res for second pass.
switch_to_hires:
        JSR switch_res
        BEQ no_more_passes  ; Switch mode failed.
        JMP nxt_pass
no_more_passes:
        JSR check_userinput ; Check userinput.
        BNE wait
        JMP first_pass      ; Recalculate new image.
wait:    
        JMP no_more_passes  ; Loop checking user input.
    
        RTS ; EXIT


; Variables in page 0 for faster access.
var_bytes   = $a0
iter        = var_bytes +  0
max_iter    = var_bytes +  1
num_tiles_w = var_bytes +  2  ; Number of tile on screen (horizontal).
num_tiles_h = var_bytes +  3  ; Number of tile on screen (vertical).
tilex       = var_bytes +  4  ; Current tile x position on screen.
tiley       = var_bytes +  5  ; Current pixel y position on screen.
pixelx      = var_bytes +  6  ; Current pixel x position in tile.
pixely      = var_bytes +  7  ; Current pixel y position in tile.
b_hist      = var_bytes +  8  ; 4x8 block: Histogram table start (indices are iters mod16).
b_hist_last = b_hist    + 16  ; 4x8 block: Histogram table end.
h_top4_cnt  = b_hist_last     ; 4x8 block: Histogram 4 top counts (pixels).
h_top4_idx  = h_top4_cnt + 4  ; 4x8 block: Histogram 4 top indices (iters mod16).
b_END       = h_top4_idx + 4  ; --------------


; Words
var_words   = b_END            ; Start of words.
;
buf_it_ptr  = var_words +  0   ; Pixel iterations buffer.
ax          = var_words +  2   ; Screen plane upper-left corner x (Q6.10).
ay          = var_words +  4   ; Screen plane upper-left corner y (Q6.10).
cx          = var_words +  6   ; Current plane point x (Q6.10).
cy          = var_words +  8   ; Current plane point x (Q6.10). 
incx_lr     = var_words + 10   ; Lo-res pixel increment x (Q6.10).
incy_lr     = var_words + 12   ; Lo-res pixel increment y (Q6.10).
incx        = var_words + 14   ; Current pixel increment x (Q6.10).
incy        = var_words + 16   ; Current pixel increment y (Q6.10).
zx          = var_words + 18   ; zx (Q6.10).
zy          = var_words + 20   ; zy (Q6.10).
zx2         = var_words + 22   ; Squared zx (Q6.10).
zy2         = var_words + 24   ; Squared zy (Q6.10).
screenw     = var_words + 26   ; Screen width (pixels).
screenh     = var_words + 28   ; Screen height (pixels).
tilew       = var_words + 30   ; Tile width (pixels).
tileh       = var_words + 32   ; Tile height (pixels).
tile_num    = var_words + 34   ; Tile number (sequential).
squares     = var_words + 36   ; Squares table pointers (Q5.9*Q5.9 = Q6.10).
str_ptr     = var_words + 38   ; Current char of string to print.
scr_ptr     = var_words + 40   ; Curren screen pointer of string print routine.
bmp_ptr     = var_words + 42   ; Curren bitmap pointer (hi-res).
cram_ptr    = var_words + 44   ; Color RAM ptr.
t_ax        = var_words + 46   ; Current tile ax (upper-left corner x).
t_ay        = var_words + 48   ; Current tile ay (upper-left corner y).

  
  
 
;------------- Check if the current hi-res tile is skippable -------------
; buf_it_ptr must point to the lo-res pixel to check in buf_iters_lr.
; Output: Set Z flag if tile is skippable (e.g. "BEQ skip").
check_skippable:
        ; Can't skip first tile in a row.
+       LDX tilex
        BNE +
        LDA #1      ; Reset Z flag.
        RTS
+       ; Can't skip last tile in a row.
        INX
        CPX num_tiles_w
        BNE +
        LDA #1      ; Reset Z flag.
        RTS
+       ; Can't skip first tile in a col.
        LDX tiley
        BNE +
        LDA #1      ; Reset Z flag.
        RTS
+       ; Can't skip last tile in a col.
        INX
        CPX num_tiles_h
        BNE +
        LDA #1      ; Reset Z flag.
        RTS
+       ; Run skip heuristics.
        CLC
        LDA #<buf_iters_lr
        ADC tile_num
        STA buf_it_ptr
        LDA #>buf_iters_lr
        ADC tile_num+1
        STA buf_it_ptr+1
        LDY #$00
        ; Get reference lo-res iters in this point.
        LDA (buf_it_ptr),Y
        TAX                ; Save iters.
        ; Dec buf_it_ptr, as indirect-indexed does not allow negative offsets.
        LDA buf_it_ptr
        BNE +
        DEC buf_it_ptr+1
+       DEC buf_it_ptr
        TXA                ; Restore iters.
        ; Now compare to iters in other lo-res pixels at cardinal directions.
        ; Check WEST
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check EAST
        INY
        INY
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check northern neighbors.
        LDA buf_it_ptr
        SEC
        SBC num_tiles_w
        STA buf_it_ptr
        LDA buf_it_ptr+1
        SBC #0
        STA buf_it_ptr+1
        TXA                 ; Restore iters.
        ; Check NORTH-EAST
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check NORTH
        DEY
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check NORTH-WEST
        DEY
        CMP (buf_it_ptr),Y
        BEQ +
        RTS        
+       ; Check southern neighbors.
        LDA num_tiles_w
        ASL                 ; num_tiles_w *= 2
        CLC
        ADC buf_it_ptr
        STA buf_it_ptr
        LDA buf_it_ptr+1
        ADC #0
        STA buf_it_ptr+1
        TXA                 ; Restore iters.
        ; Check SOUTH-WEST
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check SOUTH
        INY
        CMP (buf_it_ptr),Y
        BEQ +
        RTS
+       ; Check SOUTH-EAST
        INY
        CMP (buf_it_ptr),Y
        ; Final check done. Z flag is set with final result.
        RTS


;------------- Set bitmap pointer to next tile -------------
; Clobbered: A, X
;
bmp_to_next_tile:
.if BUILD_C64 | BUILD_C128 | BUILD_TED
        CLC
        LDA bmp_ptr
        ADC #8
        STA bmp_ptr
        BCC +
        INC bmp_ptr+1
+       RTS

.elif BUILD_ATARI
        LDX tilex
        CPX num_tiles_w
        BEQ b_end_of_row
        INC bmp_ptr
        BNE +
        INC bmp_ptr+1
+
        RTS
b_end_of_row:
        LDX #HIRES_TILE_H-1
-       LDA bmp_ptr
        CLC
        ADC num_tiles_w
        STA bmp_ptr
        LDA bmp_ptr+1
        ADC #0
        STA bmp_ptr+1
        DEX
        BNE -
        RTS

.elif BUILD_BEEB
        CLC
        LDA bmp_ptr
        ADC #16
        STA bmp_ptr
        BCC +
        INC bmp_ptr+1
+       RTS
.else   
        ; Unknown machine.
        RTS     
.endif        


;=========================================================================
; Check for user input.
; We read the machine's main joystick signals and put them in A using this format:
;   A=[xxxFRLDU] : All bits are active low: Fire, Right, Left, Down, Up.
;
; Output: Set Z flag if image must be recalculated (e.g. BEQ recalc).
; Clobbered: A, X, Y
check_userinput:
        ; Read joy inputs and translate to common format.
.if BUILD_C64 | BUILD_C128
        ; C64/C128 joystick.
        LDA $DC00           ; Get CIA1:PRA. This is [xxxFRLDU].
        AND #$1F            ; Get only joy-2 actions.

.elif BUILD_TED
        ; TED joystick input.
        LDA #$FF
        STA $FD30           ; 6529B Keyboard scan mask.
        LDA #$02            ; Select joy-1.
        STA $FF08           ; Strobe latch.
        LDA $FF08           ; Read value.
        AND #$0F            ; Mask directions.
        ; Directions are already in common format, but fire is bit 6.
        BIT $FF08           ; Check for joy-1 fire (pressed if bit 6 is 0).
        BVC +
        ORA #$10
+       

.elif BUILD_ATARI
        ; Atari joystick input.
        LDA $D300           ; PIA:PORTA[3..0] is joy-1 directions [R,L,D,U]. Active low.
        AND #$0F
        LDX $D010           ; GTIA:TRIG0[0] is joy-1 trigger. Active low.
        BEQ +               ; We can do this because TRIG0[7..1] is always 0.
        ORA #$10
+

.elif BUILD_BEEB
        ; BBC Micro analog joystick input (uPD7002 chip).
        ; First, read fire button.
        LDX #$1F            ; Use X to store joy value.
        LDA $FE40
        AND #$10            ; Bit 4 is low if joy-1 fired.
        BNE +
        LDX #$0F            ; Fire pressed.
 +      ; Now check directions.
 
        LDA beeb_joy_dir_axis   ; Get current axis (bit 0) and waiting flag (bit 7).
        BMI adc_check       ; If bit 7 is set, check if result is ready.
        ; Else, init next axis read.
adc_init:
        AND #$01            ; Select channel: 0 = Joy-1 x-axis; 1 = Joy-1 y-axis.
        STA $FEC0           ; Write to data latch / AD start reg.
        ORA #$80            ; Remember to wait for the ADC (about 4ms for 8-bit).
        STA beeb_joy_dir_axis
        JMP done_joy_BEEB

adc_check:
        LDA $FEC0           ; Read ADC status register. Bit 7 is 0 when conversion completed.
        BMI done_joy_BEEB   ; Branch if conversion not yet completed.
        ; Conversion completed.
        LDA beeb_joy_dir_axis
        TAY                 ; Save axis to Y (bit 7 is also set).
        LDA $FEC1           ; Fetch ADC value [0..255].
        CPY #$80            ; Which axis ?
        BNE +
        STA beeb_joy_last_x
        BEQ chk_thesholds   ; Unconditional.
 +      STA beeb_joy_last_y
 chk_thesholds:
        CMP #32             ; Dead-zone threshold low.
        BCS chk_hi          ; BGE
        ; Low detected.
        CPY #$80            ; Which axis ?
        BNE +
        ; RIGHT
        TXA
        AND #$F7
        TAX
        JMP switch_axis
+       ; DOWN
        TXA
        AND #$FD
        TAX
        JMP switch_axis        
        
chk_hi: CMP #256-32         ; Dead-zone threshold high.
        BCC switch_axis     ; BLT
        ; High detected.
        CPY #$80            ; Which axis ?
        BNE +
        ; LEFT
        TXA
        AND #$FB
        TAX
        JMP switch_axis
+       ; UP
        TXA
        AND #$FE
        TAX
        JMP switch_axis

switch_axis:
        ; Switch axis for next read.
        LDA beeb_joy_dir_axis
        EOR #$01
        STA beeb_joy_dir_axis
        JMP adc_init        ; Init next axis read.

done_joy_BEEB:
        ; Read keyboard.
        ; 'SHIFT'
        LDA #0
        STA $FE4F
        LDA $FE4F  ; N flag = pressed.
        BPL +
        TXA
        AND #$EF   ; Simulate fire with 'SHIFT'.
        TAX
+       ; 'RIGHT'
        LDA #121
        STA $FE4F
        LDA $FE4F  ; N flag = pressed.
        BPL +
        TXA
        AND #$F7
        TAX
+       ; 'LEFT'
        LDA #25
        STA $FE4F
        LDA $FE4F  ; N flag = pressed.
        BPL +
        TXA
        AND #$FB
        TAX
+       ; 'DOWN'
        LDA #41
        STA $FE4F
        LDA $FE4F  ; N flag = pressed.
        BPL +
        TXA
        AND #$FD
        TAX
+       ; 'UP'
        LDA #57
        STA $FE4F
        LDA $FE4F  ; N flag = pressed.
        BPL +
        TXA
        AND #$FE
        TAX
+
        ; ...

done_keyb_BEEB:
        TXA                 ; Move result to A.
        
.else
        ; Unsupported platform. No input.
        LDA #$1F
.endif
        
        CMP #$1F
        BNE yes_input
        ; No input.
        LDA #$01          ; Clear zero flag to signal no recalculation needed, and return.
        RTS
    
yes_input:
        TAX               ; Save joy input to X.
        ; Only process input at "human" time intervals.
.if BUILD_C64
        LDA $DC05         ; Get Timer A high byte.
.elif BUILD_C128
        LDA $DC07         ; Get Timer B high byte.
.elif BUILD_TED
        LDA $FF03         ; Get TED Timer #1
.elif BUILD_ATARI
        LDA frame_couter  ; Get 
.elif BUILD_BEEB
        LDA $FE45         ; Time 1 high byte.
.endif

        AND #$F0          ; Time mask.
        CMP prev_timer    ; Compare to last-input masked time.
        BNE process_input
        RTS               ; Too soon.

process_input:
        ; We have some input.
        ;INC COL_BORDER    ; Debug only.
        
        ; If any directions are pressed, then we have some action.
        TXA
        AND #$0F
        CMP #$0F
        BNE directions_pressed
        LDA #$01          ; Clear zero flag to signal no recalculation needed, and return.
        RTS


directions_pressed:
        STY prev_timer    ; Remember last-input masked time.
        TXA               ; Retreive joy input from X.
        AND #$10
        BEQ fire          ; If bit 4 is 0, then fire is pressed.
        JMP no_fire

fire:
        ; Fire button pressed. Handle zoom.
        ; Note: We have almost no checks for precision underflow.
        ;       Let's call underflow a "user error" :-)
chk_zoom_IN:
        TXA               ; Retreive joy input from X.
        AND #$01          ; Up
        BNE chk_zoom_OUT
        LDA incx_lr
        SEC
        LDA incx_lr+1
        STA incx_lr_prev+1
        LDA incx_lr
        STA incx_lr_prev
        SBC #4
        BCS +
        SEC
        LDA #1            ; Max zoom-in reached.
+       STA incx_lr
        LDA incx_lr+1
        SBC #0
        STA incx_lr+1       
        SEC
        LDA incy_lr+1
        STA incy_lr_prev+1
        LDA incy_lr
        STA incy_lr_prev
        SBC #4
        BCS +
        SEC
        LDA #1            ; Max zoom-in reached.
+       STA incy_lr
        LDA incy_lr+1
        SBC #0
        STA incy_lr+1
        JSR recenter      ; This clobbers X.  
        LDA incx_lr
        JSR print_A_hex
        JMP end_input

chk_zoom_OUT:    
        TXA               ; Retrieve input.
        AND #$02          ; Down
        BNE chk_iters_more
        LDA incx_lr+1
        STA incx_lr_prev+1
        LDA incx_lr
        CMP #241          ; Max zoom-out reached.
        BCC +
        JMP end_input
+       STA incx_lr_prev
        CLC
        ADC #4
        STA incx_lr
        LDA incx_lr+1
        ADC #0
        STA incx_lr+1       
        LDA incy_lr+1
        STA incy_lr_prev+1
        CLC
        LDA incy_lr
        STA incy_lr_prev
        ADC #4
        STA incy_lr
        LDA incy_lr+1
        ADC #0
        STA incy_lr+1
        LDA incx_lr
        JSR print_A_hex
        JSR recenter      ; This clobbers X.  
        JMP end_input
+    
chk_iters_more:
        TXA               ; Retrieve input.
        AND #$08          ; Right
        BNE chk_iters_less
        LDA max_iter
        CMP #255          ; Max 255 iters.
        BEQ end_zoomiters
        INC max_iter
        LDA max_iter
        JSR print_A_hex

chk_iters_less:
        TXA               ; Retrieve input.
        AND #$04          ; Left
        BNE end_zoomiters
        DEC max_iter
        BNE +
        LDA #2            ; Min iters is 2.
        STA max_iter
+       LDA max_iter
        JSR print_A_hex

end_zoomiters:
        ; Set zero flag if image must be recalculated.
        TXA
        AND #$0F           
        CMP #$0F          ; Check if a joystick direction was pressed.    
        BEQ no_dir
        LDA #$00
        RTS
no_dir:
        LDA #$FF
        RTS

no_fire: ;----- no fire button pressed
        ; Handle pan.
chk_pan_U:
        TXA               ; Retrieve input from X.
        AND #$01          ; Up
        BNE chk_pan_D
        CLC
        LDA ay
        ADC incy
        STA ay
        LDA ay+1
        ADC incy+1
        STA ay+1      
chk_pan_D:
        TXA               ; Retrieve input.
        AND #$02          ; Down
        BNE chk_pan_L
        SEC
        LDA ay
        SBC incy
        STA ay
        LDA ay+1
        SBC incy+1
        STA ay+1
chk_pan_L:
        TXA               ; Retrieve input.
        AND #$04          ; Left
        BNE chk_pan_R
        SEC
        LDA ax
        SBC incx
        STA ax
        LDA ax+1
        SBC incx+1
        STA ax+1
chk_pan_R:
        TXA               ; Retrieve input.
        AND #$08          ; Right
        BNE end_input
        CLC
        LDA ax
        ADC incx
        STA ax
        LDA ax+1
        ADC incx+1
        STA ax+1
  
    
end_input:
        ;JSR print_A_hex
        ; Set zero flag to signal image must be recalculated.
        LDA #$00
        RTS

prev_timer:
        .byte 0

;===================================================================
; Recenter mandelbrot based on newly changed incx and incy.
; This will readjust ax and ay.
;
; Inputs:
;   prev_incx : Previous incx
;   prev_incy : Previous incy
;
; Outputs:
;   ax : New upper-left x coord bnased on current incx
;   ay : New upper-left y coord bnased on current incy

recenter:
        ; Calculate current complex plane width.
        LDA screenw
        STA x0
        LDA screenw+1
        STA x1
        LDA incx_lr
        STA y0
        LDA incx_lr+1
        STA y1
        JSR multiply_16bit_unsigned
        LDA z0
        STA pw_cur
        LDA z1
        STA pw_cur+1
        ; Calculate previous complex plane width.
        LDA incx_lr_prev
        STA y0
        LDA incx_lr_prev+1
        STA y1
        JSR multiply_16bit_unsigned
        LDA z0
        STA pw_prev
        LDA z1
        STA pw_prev+1
        
        ; Calculate current complex plane height.
        LDA screenh
        STA x0
        LDA screenh+1
        STA x1
        LDA incy_lr
        STA y0
        LDA incy_lr+1
        STA y1
        JSR multiply_16bit_unsigned
        LDA z0
        STA ph_cur
        LDA z1
        STA ph_cur+1
        ; Calculate previous complex plane height.
        LDA incy_lr_prev
        STA y0
        LDA incy_lr_prev+1
        STA y1
        JSR multiply_16bit_unsigned
        LDA z0
        STA ph_prev
        LDA z1
        STA ph_prev+1    
        
        ; Compute diff.
        SEC
        LDA pw_prev
        SBC pw_cur
        STA pw_diff
        LDA pw_prev+1
        SBC pw_cur+1
        STA pw_diff+1
        SEC
        LDA ph_prev
        SBC ph_cur
        STA ph_diff
        LDA ph_prev+1
        SBC ph_cur+1
        STA ph_diff+1
        ; Divide diff by two.
        CMP #$80        ; Set carry if negative.
        ROR pw_diff+1
        ROR pw_diff
        CMP #$80        ; Set carry if negative.
        ROR ph_diff+1
        ROR ph_diff
        ; Add halved diff to ax.
        CLC
        LDA ax
        ADC pw_diff
        STA ax
        LDA ax+1
        ADC pw_diff+1
        STA ax+1
        ; Sub halved diff from ay (pixel vs plane coords are opposite on y-axis).
        SEC
        LDA ay
        SBC ph_diff
        STA ay
        LDA ay+1
        SBC ph_diff+1
        STA ay+1
    
        RTS

incx_lr_prev: .word 0
incy_lr_prev: .word 0
pw_cur:       .word 0
ph_cur:       .word 0
pw_prev:      .word 0
ph_prev:      .word 0
pw_diff:      .word 0
ph_diff:      .word 0


;===========================================================
; Print A as a hex number on the upper-left corner of the screen.
; NOTE: Registers are preserved.

print_A_hex:
        PHA        ; Save A.
        PHA        ; Save A.
        
        LSR
        LSR
        LSR
        LSR
        CMP #$0A
        BCS pA_alpha_0
        ; Not alpha, i.e. [0..9]
        ADC #$30 + 9
pA_alpha_0:    
        SEC
        SBC #9
        STA nibble_char_h

nxt_nibble:
        PLA        ; Restore A.
        AND #$0F
        CMP #$0A
        BCS pA_alpha_1
        ; Not alpha, i.e. [0..9]
        ADC #$30 + 9
pA_alpha_1:    
        SEC
        SBC #9
        STA nibble_char_l
        
        ;LDA mode
        ;AND #MODE_VIC2 | MODE_KAWARI
        ;BEQ +
        ; Output.
        LDA nibble_char_h
        STA SCR_RAM
        LDA nibble_char_l
        STA SCR_RAM+1
.if COL_RAM
        ; Set color.
        LDA #$61   ; Light gray (use also high nibble for TED machines).
        STA COL_RAM
        STA COL_RAM+1
.endif
        JMP end_pAh
;+ 
        ;CMP #MODE_VDC
        ;BNE +
        ;; Output.
        ;TXA
        ;PHA
        ;LDX #$12          ; VDC mem addr HI
        ;LDA #$00
        ;JSR vdc_write
        ;INX               ; VDC mem addr LO
        ;JSR vdc_write
        ;LDX #$1F          ; VDC data
        ;LDA nibble_char_h
        ;JSR vdc_write
        ;LDA nibble_char_l
        ;JSR vdc_write
        ;; Set color.
        ;LDX #$12          ; VDC mem addr HI
        ;LDA #$10
        ;JSR vdc_write
        ;INX               ; VDC mem addr LO
        ;LDA #$00
        ;JSR vdc_write
        ;LDX #$1F          ; VDC data
        ;LDA #$1F          ; White.
        ;JSR vdc_write
        ;PLA
        ;TAX
        ;JMP end_pAh
;+
end_pAh:    
        PLA        ; Restore A.
        RTS

nibble_char_h: .byte 0
nibble_char_l: .byte 0


;=============================================================
; Description: Signed Q5.9 fixed-point squares table.
; This is a 32KB table containing 16384 Q5.9 numbers (two bytes each).
; Only positive Q5.9 with even lowest bit are present.
; Table starts at $5000 and ends at $cfff

init_squares_q5_9:
        LDA #$00
        STA squares     ; Use a page 0 address.
        LDA #$50
        STA squares+1
        LDY #$00        ; Used for indirect indexed.
        STY x0          ; Start from 0.
        STY x1
        STY y0
        STY y1

-       JSR multiply_Q6_10_signed
        ; Result is in [z1..z2].
        LDA z1
        LDY #$00
        STA (squares),Y
        INC squares
        LDA z2
        STA (squares),Y
        INC squares
        BNE +
        INC squares+1
+       INC x0      ; Skip odd numbers.
        INC x0
        INC y0
        INC y0
        BNE -
        INC x1
        INC y1
        LDA x1
        CMP #$80    ; Last entry is for number $7FFF.
        BNE -

squares_tab_complete:
        RTS

;===========================================================
; Description: Get the square of the given signed Q10.6 number using the signed Q5.9 squares table.
; NOTE: The square will be an approximation if the last bit is odd.
;
; Input: Q6.10 signed value in x0,x1
;
; Output: Approximated Q6.10 signed squared value in z1,z2

square_Q10_6:
        ; Check if negative.
        LDA x1
        BPL not_neg
        
        ; Convert to positive (exact).
        SEC
        LDA #$00
        SBC x0
        AND #$FE            ; Make even.
        STA squares
        LDA #$00
        SBC x1
        STA squares+1
        BPL fetch_square   ; Always positive (i.e. branch always).
not_neg:
        LDA x0
        AND #$FE            ; Make even.
        STA squares
        LDA x1
        STA squares+1
fetch_square:        
        ; Fetch from table.
        CLC
        LDA squares+1
        ADC #$50            ; Table offset
        STA squares+1
        LDY #$00
        LDA (squares),Y
        STA z1
        INY
        LDA (squares),Y
        STA z2
        RTS

;==============================================================
; Description: Signed Q6.10 fixed-point multiplication with signed Q6.10 result.
; This uses the multiply_16bit_unsigned routine.
;
; Revision history [authors in square brackets]:
; 2024-11-07: First simple test loop. [DDT]
;
; Input: Q6.10 signed value in x0,x1
;        Q6.10 signed value in y0,y1
;
; Output: Q6.10 signed value z1,z2
;
; Clobbered: X, A, C
multiply_Q6_10_signed:
            
        ; Step 1: signed multiply
        JSR multiply_16bit_signed
        ; Result is in z1,z2.
        
        ; Perform fixed point adjustment.
        ; We need to shift it right 10 bits, so just ignore z0 and shift right twice the 24 bit value in z1,z2,z3.
        LDA z3
        CMP #$80        ; Set carry if result is negative.
        ROR z3
        ROR z2
        ROR z1
        
        CMP #$80        ; Set carry if result is negative.
        ROR z3
        ROR z2
        ROR z1
        
        RTS

buf_iters_hr:              .fill 4*8    ; We buffer hi-res tiles up to 4x8.
buf_tile_size:             .byte 0      ; 0 means screen size or none (depending on mode).
; Tile iterations buffer (max 40*32).
; This is only used in hi-res. In lo-res we write colors directly to Color RAM.
.if BUILD_C64
buf_iters_lr = $E000
.elif BUILD_C128
buf_iters_lr = $E000
.elif BUILD_TED
buf_iters_lr = $E000
.elif BUILD_PET
buf_iters_lr = $1C00            ; Is that ok ?
.elif BUILD_ATARI
buf_iters_lr = $E000
.elif BUILD_BEEB
buf_iters_lr = $1000
.endif


; ---------------- Signed 16x16=32 bits ----------------
; Revision history [authors in square brackets]:
;   2024-11-07: Original smult12 code taken from:
;    https://github.com/TobyLobster/multiply_test
;   2024-11-07: Adapted to TASS and Commodore 64. [DDT];
;
; smult12.a
; from smult3, but tweaked to use mult86 as a base
;
; 16 bit x 16 bit signed multiplication, 32 bit result
; Average cycles: 234.57
; 2210 bytes (including init routine)

; pointers to tables of squares
p_sqr_lo1    = $8b   ; 2 bytes
p_sqr_hi1    = $8d   ; 2 bytes
p_neg_sqr_lo = $8f   ; 2 bytes
p_neg_sqr_hi = $91   ; 2 bytes
p_sqr_lo2    = $93   ; 2 bytes
p_sqr_hi2    = $95   ; 2 bytes

; the inputs and outputs
x0  = p_sqr_lo1      ; multiplier, 2 bytes
x1  = p_sqr_lo2      ; [WARNING: note non-consecutive bytes!]
y0  = $04            ; multiplicand, 2 bytes
y1  = $05
z0  = $06            ; product, 4 bytes
z1  = $07            ;
z2  = $08            ;
z3  = $09            ;



; IMPORTANT: Align each table to start of a page.
.if BUILD_C64
mul_tab_offset = $4700
.elif BUILD_C128
mul_tab_offset = $4700
.elif BUILD_TED
mul_tab_offset = $4700
.elif BUILD_PET
mul_tab_offset = $2000
.elif BUILD_ATARI
mul_tab_offset = $4700
.elif BUILD_BEEB
mul_tab_offset = $2700
.endif

; Note - the last byte of each table is never referenced, as a+b<=510
        * = mul_tab_offset
sqrlo:
    .for i := 0, i < 511, i += 1
        .byte <((i*i)/4)
    .endfor

        * = mul_tab_offset + $200
sqrhi:
    .for i := 0, i < 511, i += 1
        .byte >((i*i)/4)
    .endfor

        * = mul_tab_offset + $400
negsqrlo:
    .for i := 0, i < 511, i += 1
        .byte <(((255-i)*(255-i))/4)
    .endfor

        * = mul_tab_offset + $600
negsqrhi:
    .for i := 0, i < 511, i += 1
        .byte >(((255-i)*(255-i))/4)
    .endfor

    
; Description: Signed 16-bit multiplication with signed 32-bit result.
;
; Input: 16-bit signed value in x0,x1
;        16-bit signed value in y0,y1
;
; Output: 32-bit signed value in z0,z1,z2,z3
;
; Clobbered: X, A, C
multiply_16bit_signed:
   
    ; Step 1: unsigned multiply
    jsr multiply_16bit_unsigned

    ; Step 2: Apply sign (See C=Hacking16 for details).
    bit x1
    bpl x1_pos
    sec
;    lda z2
    sbc y0
    sta z2
    lda z3
    sbc y1
    sta z3
x1_pos:
    bit y1
    bpl y1_pos
    sec
    lda z2
    sbc x0
    sta z2
    lda z3
    sbc x1
    sta z3
y1_pos:

    rts


; mult86.a
; from 6502.org, by Repose: http://forum.6502.org/viewtopic.php?p=106519#p106519
;
; 16 bit x 16 bit unsigned multiply, 32 bit result
; Average cycles: 193.07 (including z1 and z2 results being saved to memory locations)

; How to use:
; call jsr init, before first use
; put numbers in (x0,x1) and (y0,y1) and result is (z3, z2 (also A), z1 (also Y), z0)

; Diagram of the additions
;                 y1    y0
;              x  x1    x0
;                 --------
;              x0y0h x0y0l
; +      x0y1h x0y1l
; +      x1y0h x1y0l
; +x1y1h x1y1l
; ------------------------
;     z3    z2    z1    z0

multiply_16bit_unsigned:
    ; set multiplier as x1
    lda x1
    sta p_sqr_hi1
    eor #$ff
    sta p_neg_sqr_lo
    sta p_neg_sqr_hi

    ; set multiplicand as y0
    ldy y0

    ; x1y0l =  low(x1*y0)
    ; x1y0h = high(x1*y0)
    sec
    lda (p_sqr_lo2),y
    sbc (p_neg_sqr_lo),y
    sta x1y0l+1
    lda (p_sqr_hi1), y
    sbc (p_neg_sqr_hi),y
    sta x1y0h+1

    ; set multiplicand as y1
    ldy y1

    ; x1y1l =  low(x1*y1)
    ; z3    = high(x1*y1)
    lda (p_sqr_lo2),y
    sbc (p_neg_sqr_lo),y
    sta x1y1l+1
    lda (p_sqr_hi1),y
    sbc (p_neg_sqr_hi),y
    sta z3

    ; set multiplier as x0
    lda x0
    sta p_sqr_hi2
    eor #$ff
    sta p_neg_sqr_lo
    sta p_neg_sqr_hi

    ; x0y1l =  low(x0*y1)
    ; X     = high(x0*y1)
    lda (p_sqr_lo1),y
    sbc (p_neg_sqr_lo),y
    sta x0y1l+1
    lda (p_sqr_hi2),y
    sbc (p_neg_sqr_hi),y
    tax

    ; set multiplicand as y0
    ldy y0

    ; z0    =  low(x0*y0)
    ; A     = high(x0*y0)
    lda (p_sqr_lo1),y
    sbc (p_neg_sqr_lo),y
    sta z0
    lda (p_sqr_hi2),y
    sbc (p_neg_sqr_hi),y

    clc
do_adds:
    ; add the first two numbers of column 1
x0y1l:
    adc #0      ; x0y0h + x0y1l
    tay

    ; continue to first two numbers of column 2
    txa
x1y0h:
    adc #0      ; x0y1h + x1y0h
    tax         ; X=z2 so far
    bcc +
    inc z3      ; column 3
    clc

    ; add last number of column 1
+
    tya
x1y0l:
    adc #0      ; + x1y0l
    tay         ; Y=z1

    ; add last number of column 2
    txa
x1y1l:
    adc #0      ; + x1y1l
    bcc fin     ; A=z2
    inc z3      ; column 3
fin:
    sta z2      ; Added: save middle byte results from registers A,Y
    sty z1      ;
    rts

; Once only initialization
; (TODO: this could set up the pointer values in a loop to save memory
; it could also generate the square tables in code rather than load them)
mulu_init:
    lda #>sqrlo
    sta p_sqr_lo2+1
    sta p_sqr_lo1+1

    lda #>sqrhi
    sta p_sqr_hi1+1
    sta p_sqr_hi2+1

    lda #>negsqrlo
    sta p_neg_sqr_lo+1

    lda #>negsqrhi
    sta p_neg_sqr_hi+1
    rts
    
END_ADDRESS:
    
