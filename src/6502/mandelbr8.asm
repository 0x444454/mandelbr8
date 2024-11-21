; Mandelbr8 (6502 version).
; DDT's fixed-point Mandelbrot 8-bit generator.
;
; https://github.com/0x444454/mandelbr8
;
; Revision history [authors in square brackets]:
;   2024-11-07: First simple test loop. [DDT]
;   2024-11-08: Unoptimized C64 version. [DDT]
;   2024-11-09: Added support for C128 and 80x50 VDC mode. [DDT]
;   2024-11-10: Added Q9.10 squares table for speedup. [DDT]
;   2024-11-12: VIC-II Kawari: Support HW multiplier. [DDT]
;   2024-11-13: Support Commodore Plus/4. [DDT]
;   2024-11-15: Support C64 multicolor bitmap (second-pass). [DDT]
;   2024-11-16: Multicolor histogram optimized rendering. [DDT]
;   2024-11-17: Added C64 tile-skip heuristics. [DDT]
;   2024-11-18: Support TED multicolor bitmap (second-pass). [DDT]
;   2024-11-20: Support Kawari custom palette. [DDT]


; Enable *only* the build you need (set to 1).
BUILD_C64  = 1 ; Commodore 64 (or C128 in C64 mode).
BUILD_C128 = 0 ; Commodore 128
BUILD_TED  = 0 ; Commodore TED machines: Plus/4 and C16 with 64 KB.
;BUILD_PET  = 0 ; WARNING: Commodore PET machines ARE NOT YET SUPPORTED !!!.

.if BUILD_C64
  * = $0801   ; C64
.elif BUILD_C128
  * = $1C01   ; C128
.elif BUILD_TED
  * = $1001   ; TED machines (C16 and Plus/4)
.elif BUILD_PET
  * = $401    ; PET.
.endif
  

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
        
main:
        SEI        ; Disable interrupts.
        
.if BUILD_C128
        ; Configure C128 MMU. Kick ROM out of the way.
        LDA #$3E            ; All Bank0 RAM but keep I/O regs ($D000-$DFFF).
        STA $D500
.endif

        ; Initialize unsigned mul tables. This is required also by "print_str", so do it now.
        JSR mulu_init 

        ; Print intro message (first message printed must start with $93=CLS).
        LDA #<str_intro
        STA str_ptr
        LDA #>str_intro
        STA str_ptr+1
        JSR print_str
       
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
.endif ;BUILD_C128
        

.if BUILD_C128
        ; Detect C128.
        LDA $D030
        AND #$02           ; Check VIC-IIe test bit (should be 0 on a C128).
        BNE no_C128        ; This is not a C128.
        LDA #SYS_C128
        STA system
        
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
        LDA #$ff
        STA $dc02 ; DDRA all R/W
        STA $dc03 ; DDRB all R/W
        
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
.endif ;BUILD_C64 | BUILD_C128 

.if BUILD_TED
        ; Unmap ROM out of the way.
        LDA #$00   ; Dummy value.
        STA $FF3F  ; Any write to this register unmaps ROM.

        ; Setup TED
        LDA #$00
        STA COL_BORDER
        LDA #$00
        STA COL_BGND
.endif ;BUILD_TED
        
        ; Print wait msg.
        LDA #<str_wait
        STA str_ptr
        LDA #>str_wait
        STA str_ptr+1
        JSR print_str
        
        ; Only build squares table if we have at least 64 KB RAM.
.if BUILD_C64 | BUILD_C128 | BUILD_TED
        ; Initialize Q5.9 squares table.
        JSR init_squares_q5_9
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
        .text "version 2024-11-20", $0D
        ;.text "https://github.com/0x444454/mandelbr8", $0D
        .byte $00

str_kawari:
        .text "found kawari.", $0D
        .byte $00

str_wait:
        .text "please wait...", $0D
        .byte $00
           
;------------- system & configs -------------

.if BUILD_C64
  SCR_RAM    = $0400
  COL_RAM    = $D800
  COL_BORDER = $D020
  COL_BGND   = $D021
.elif BUILD_C128
  SCR_RAM    = $0400
  COL_RAM    = $D800
  COL_BORDER = $D020
  COL_BGND   = $D021
.elif BUILD_TED
  SCR_RAM    = $0C00 ; Video matrix
  COL_RAM    = $0800 ; Attributes matrix
  COL_BORDER = $FF19
  COL_BGND   = $FF15 ; Background register #0
.elif BUILD_PET
  SCR_RAM    = $8000 ; Video matrix.
  COL_RAM    = 0     ; No color RAM. Unfortunately, TASS64 has no .ifdef.
  COL_BORDER = $FFFF ; Dummy.
  COL_BGND   = $FFFF ; Dummy.
.endif

SYS_C64   = 0
SYS_C128  = 1

; The following is a bitmask, for faster checks.
MODE_VIC2     =   $01
MODE_KAWARI   =   $02
MODE_VDC      =   $04

system:  .byte SYS_C64
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
-       LDA #$20           ; Space
        STA SCR_RAM,X
        STA SCR_RAM+$100,X
        STA SCR_RAM+$200,X
        STA SCR_RAM+$300,X
.if COL_RAM
        LDA #$71           ; Upper nibble is only used on TED machines.
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
        BPL +
        ADC #128
        JMP done_conv
+       CMP #64
        BMI done_conv
        CMP #97
        BPL +
        SEC
        SBC #64
        JMP done_conv
+       CMP #128
        BPL done_conv
        SEC
        SBC #32
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
        LDA mode
        AND #MODE_VIC2 | MODE_KAWARI
        BEQ +
        ; Clear screen (and some more :-) with all reverse spaces.
        LDX #$00
        LDA #$A0
-       STA SCR_RAM,X
        STA SCR_RAM+$100,X
        STA SCR_RAM+$200,X
        STA SCR_RAM+$300,X
        INX
        BNE - ; Print till zero term or max 256 chars.
        ; Clear attribs (and some more) to white.
        LDX #$00
        LDA #$01
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
        LDA mode
        AND #MODE_VDC
        BEQ +
        JSR vdc_cls
        RTS
+   
    
        RTS
    
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
;    LDA #$00
;    RTS ; Not yet supported.
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
.else
        ; Unhandled mode.
        LDA #0          ; Return ERROR
.endif
end_applymode:
        RTS

;------------- Clear bitmap -------------
; Clear the bitmap ($2000 to $3FFF).
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


;------------- Render tile -------------
; Render a multicolor 4x8 tile based on the computed histogram.
; We need to choose between black or any of the other three most used colors in the tile.
; In multicolor bitmap, each pixel is encoded as two bits:
;   00 : Background color 0 ($D021)
;   01 : Color from bits 4-7  of c-data (Screen RAM hi-nibble).
;   10 : Color from bits 0-3  of c-data (Screen RAM lo-nibble).
;   11 : Color from bits 8-11 of c-data (Color RAM lo-nibble). 

render_tile:
        ; Set most used three colors in c-data (fourth is always black).
.if BUILD_C64 | BUILD_C128
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
        
        ; bmp_ptr contains the current tile bitmap line (4 pixels, 2 bits each).
        ; buf_it_ptr is the per-pixel iterations buffer.
        LDA #<buf_iters_hr
        STA buf_it_ptr
        LDA #>buf_iters_hr
        STA buf_it_ptr+1
        LDY #0           ; Current tile pixel [0..32].
nxt_tile_line:
        LDA #$00         ; Preset line all black.
        TAX
        STA (bmp_ptr,X)
        LDX #3           ; Tile line pixel countdown [3..0]. We use this also as a shift counter.
nxt_tile_pix:
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
        BPL nxt_tile_pix
        ; End of tile bitmap line.
        INC bmp_ptr      ; Inc bitmap ptr.
        BNE +
        INC bmp_ptr+1
+       CPY buf_tile_size
        BNE nxt_tile_line
        RTS

tmp_A: .byte 0
tmp_X: .byte 0


;------------- Mandelbrot calculation -------------
        * = $4000

Mandelbrot:
        ; Max iters
        LDA #16
        STA max_iter
    
        ; Default coordinates (fixed_point, *1024).
        start_ax = -2200
        start_ay = 1200
        
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
        AND #MODE_VIC2 | MODE_KAWARI
        BEQ +
        LDA #100
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
        AND #MODE_VIC2 | MODE_KAWARI
        BNE +
        JMP no_VIC2_setup
        ;------ VIC2
+       LDA res
        BNE hi_res
        ; VIC2 lo-res (one single 40x25 tile).
        LDA #1
        STA num_tiles_w
        STA num_tiles_h
        LDA #0
        STA tile_num
        STA tile_num+1
        STA screenw+1
        STA screenh+1
        LDA #40
        STA screenw
        STA tilew
        LDA #25
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
        LDA #$00
        STA bmp_ptr
        LDA #$20
        STA bmp_ptr+1
        LDA #160
        STA screenw
        LDA #200
        STA screenh
        LDA #0
        STA screenw+1
        STA screenh+1
        LDA #40
        STA num_tiles_w
        LDA #25
        STA num_tiles_h
        LDA #4
        STA tilew
        LDA #8
        STA tileh
        LDA #32             ; A hi-res tile is 4x8=32 pixels.
        STA buf_tile_size
        ; Update incs for second-pass.
        LDX #2        ; Rotate twice to divide lo-res incx by 4.
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
        CLC ; Not needed
        ROR
        CLC
        ADC ay
        STA ay
        LDA ay+1
        ADC #0
        STA ay+1
        JMP first_tile
no_VIC2_setup:

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
        CLC
        LDA bmp_ptr
        ADC #8
        STA bmp_ptr
        BCC +
        INC bmp_ptr+1
+       JMP skip_tile       ; Skip this tile.

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
        ;JSR multiply_Q6_10_signed ; [z1..z2] = zx*zx
        JSR square_Q10_6
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
        ;JSR multiply_Q6_10_signed ; [z1..z2] = zy*zy
        JSR square_Q10_6
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
        JSR build_histogram
        JSR scan_histogram
        JSR render_tile

skip_tile:
        ; Calc next tile.
        INC tile_num
        BNE +
        INC tile_num+1
+       INC tilex
        LDA tilex
        CMP num_tiles_w
        BNE nxt_tile_in_row
        ; End of tile row, advance to next row.
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
        LDX #8         ; Sub 8*incy to t_ay.
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
        LDX #4         ; Add 4*incx to t_ax.
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
+       LDY #$00
        ; Get reference lo-res iters in this point.
        LDA (buf_it_ptr),Y
        TAX                ; Save iters.
        ; Dec buf_it_ptr, as indirect-indexed does not allow negative offsets.
        SEC
        LDA buf_it_ptr
        SBC #1
        STA buf_it_ptr
        LDA buf_it_ptr+1
        SBC #0
        LDA buf_it_ptr+1
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
        TAX                 ; Save iters.
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
        TAX                 ; Save iters.
        ASL num_tiles_w     ; num_tiles_w *= 2
        CLC
        LDA buf_it_ptr
        ADC num_tiles_w
        STA buf_it_ptr
        LDA buf_it_ptr+1
        ADC #0
        STA buf_it_ptr+1
        ROR num_tiles_w     ; num_tiles_w /= 2
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


;=========================================================================
; Check for user input.
; Set Z flag if image must be recalculated.
check_userinput:
.if BUILD_C64 | BUILD_C128
        ; Early exit test: check for no joy-2 actions.
        LDA $DC00        ; Get CIA1 PRA
        AND #$1F         ; Any joy-2 action ?
        CMP #$1F
        BEQ no_input     ; No keyboard or joy-2 action.
.elif BUILD_TED
        LDA #$02          ; Select joy-1.
        STA $FF08         ; Strobe latch.
        LDA $FF08
        AND #$4F           ; Any joy-1 action ?
        CMP #$4F
        BEQ no_input      ; No keyboard or joy-2 action.
.endif    

    ; Only process input at "human" time intervals.
.if BUILD_C64
        LDA $DC05         ; Get Timer A high byte.
.elif BUILD_C128
        LDA $DC07         ; Get Timer B high byte.
.elif BUILD_TED
        LDA $FF03         ; Get TED Timer #1
.endif

        AND #$F0          ; Time mask.
        CMP prev_timer    ; Compare to previous value read.
        BNE input

no_input:    
        ; Enforce input delay.
        ; Clear zero flag to signal no recalculation needed.
        LDA #$01
        RTS
    
input:
        STA prev_timer    ; Save current time.
        ; We have some input.
        INC COL_BORDER    ; Debug only.

.if BUILD_C64 | BUILD_C128
        LDA $DC00         ; Get CIA1 PRA (joy-2).
        TAX               ; Save to X.
        AND #$10          ; Check for joy-2 fire (pressed if bit 4 is 0).
.elif BUILD_TED
        LDA #$FF
        STA $FD30         ; 6529B Keyboard scan mask.
        LDA #$02          ; Select joy-1.
        STA $FF08         ; Strobe latch.
        LDA $FF08         ; Read value.
        TAX               ; Save to X.
        ; Directions are as in C64, but fire is bit 6.
        AND #$40          ; Check for joy-1 fire (pressed if bit 6 is 0).
.endif
        BEQ fire          ; If bit 4 is 0, then fire is pressed.
        JMP no_fire

fire:
        ; Fire button pressed. Handle zoom.
        ; Note: We have almost no checks for precision underflow.
        ;       Let's call underflow a "user error" :-)
chk_zoom_IN:
        TXA               ; Retreive input.
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
        TXA               ; Retrieve input.
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
        ; Calculate current imaginary plane width.
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
        ; Calculate previous imaginary plane width.
        LDA incx_lr_prev
        STA y0
        LDA incx_lr_prev+1
        STA y1
        JSR multiply_16bit_unsigned
        LDA z0
        STA pw_prev
        LDA z1
        STA pw_prev+1
        
        ; Calculate current imaginary plane height.
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
        ; Calculate previous imaginary plane height.
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
; Tile iterations buffer (40*25).
; This is only used in hi-res. In lo-res we write colors directly to Color RAM.
.if BUILD_C64
buf_iters_lr = $E000
.elif BUILD_C128
buf_iters_lr = $E000
.elif BUILD_TED
buf_iters_lr = $E000
.elif BUILD_PET
buf_iters_lr = $1C00            ; Is that ok ?
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
mul_tab_offset = $0         ; Not available.
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
    