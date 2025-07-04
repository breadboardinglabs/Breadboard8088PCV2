;==============================================================================================================================
; CGAtest2.asm - Breadboard PC V2 8086 CGA Video Controller Test 1 CRTC & Colour Corrected CGA Colours based on MDATest7.asm & V1 CGATest1.asm in CS 6000
; nasm -f srec -o CGAtest2.srec -l CGAtest2.lst CGAtest2.asm

; Version          Date            Description
;  1.0             11/06/25        Based on MDATest7 & V1 CGATEst1 Initialises CRTC and Video DAC
;                                  Note extra NOP before out as middle IOW was not being output by 80C88 load in CS 6000 ES 6000
;                                  Adjust Vertical Sync timing, change palette 1 to Blue
         
                      ; Flags Register
                      ; CC  15 14 13 12 11 10 9  8  7  6  5  4  3  2  1  0
                      ;     -  -  -  -  OF DF IF TF SF ZF -  AF -  PF -  CF
                      ;
                      ; 0 CF Carry Flag
                      ; 1 -
                      ; 2 PF Parity Flag
                      ; 3 -
                      ; 4 AF Aux. Carry Flag
                      ; 5 -
                      ; 6 ZF Zero Flag
                      ; 7 SF Sign Flag
                      ; 8 TF Trap Flag
                      ; 9 IF Interrupt Flag
                      ;10 DF Direction Flag
                      ;11 OF Overflow Flag
                      ;12-15 - 
                      
cpu	8086

; This macro includes the setloc macro to pad code with FF to a given address
;%include "macro.inc"

seg0                          EQU 0x7E
seg1                          EQU 0x06
seg2                          EQU 0x5B
seg3                          EQU 0x1F
seg4                          EQU 0x27
seg5                          EQU 0x3D
seg6                          EQU 0x7D
seg7                          EQU 0x0E
seg8                          EQU 0x7F
seg9                          EQU 0x3F
segA                          EQU 0x6F
segB                          EQU 0x75
segC                          EQU 0x78
segD                          EQU 0x57
segE                          EQU 0x79
segF                          EQU 0x69
segG                          EQU 0x7C
segH                          EQU 0x67
segI                          EQU 0x60
segL                          EQU 0x70
segM                          EQU 0x6E ; (upside down U)
segN                          EQU 0x6E ; (upside down U)
segO                          EQU 0x7E
segP                          EQU 0x6B
segS                          EQU 0x3D ; (Same as 5)
segT                          EQU 0x71
segU                          EQU 0x76
segX                          EQU 0x67
segY                          EQU 0x37
segDash                       EQU 0x01
segSpace                      EQU 0x00

%define	START		0x0000		; Default Physical Address is 70000 [7000:0000]

%define DATASEG 0x6000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define USERCODESEG 0x6000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define STACKSEG 0x7000   ; Stack Segment is current top of 512K RAM minus 256 bytes for Monitor use
%define MONCODESEG 0xF000 ; Monitor Code Segment
%define GETKEY 0xC6A1     ; Address in EEPROM of far_getkey which calls DISPRESH while waiting for a key 36A1 6=F add 9 = C6A1
%define	DBUF		0xFFF0		; Display Buffer DBUF starts at offset FFF0 Physical Address is 7FFF0 [7000:FFF0] at top of 512K RAM
%define STACKSTART 0x8000 ;
org	START		; Use only upper 32 KiB of ROM

;   Reset Handler
teststart:
;CRCCONTROL          EQU  0x038
;CRCCSTATUS          EQU  0x038A4

CRTCAR              EQU  0x03d4
CRTCDR              EQU  0x03d5
VRAMSEG             EQU  0xB800
VRAM                EQU  0x0000
VRAMTOP             EQU  0x0FFF ; For CGA80x25 text mode on first page

CRTC_DAC_ADD_WR     EQU  0x03d0
CRTC_DAC_COL_VALUE  EQU  0x03d1
CRTC_DAC_PIXEL_MASK EQU  0x03d2
CRTC_DAC_ADD_RD     EQU  0x03d3
CGA_CONTROL         EQU  0x03d8
CGA_COLOUR          EQU  0x03d9
CGA_STATUS          EQU  0x03dA


ASCII0              EQU  0x30
ASCIISPACE          EQU  0x20
COLSPERROW          EQU  0x50
ROWSPERPAGE         EQU  0x19
ROWSPERPAGEBCD      EQU  0x25
BYTESPERCHAR        EQU  02
CHARATTRIB          EQU  0b00001111 ; Black background, White/Green foreground, No Blink

EOS                 EQU  0x00 ;      end of string

                    ORG  0x0000
                    
CRTStart:
                  cli
; Put something on LED Display, disabled for now
                  mov di, DBUF
                  mov byte [ds:di], segT
                  inc di
                  mov byte [ds:di], segE
                  inc di
                  mov byte [ds:di], segS
                  inc di
                  mov byte [ds:di], segT
                  inc di
                  mov byte [ds:di], seg1
                  inc di
                  mov byte [ds:di], segSpace
                  inc di
                  mov byte [ds:di], segSpace
                  inc di
                  mov byte [ds:di], segSpace ; TEST1
                  ;call MONCODESEG:GETKEY ; Return from GetKey messes up stack etc disable for now.

				  mov sp, STACKSTART ; Set stack about half way through segment, palette values were getting overwritten
                  mov ax, DATASEG
                  mov ds, ax     ; Set ds to 0x6000
                  mov ax, VRAMSEG
                  mov es, ax     ; Set es to CGA frame buffer B8000 [B800:0000]
                  xor ax, ax     ; A=al, B=ah
                  mov si, CRTCTAB 
				  
CRTCLOOP:         mov al, ah     ;stb      CRTCAR      
                  mov dx, CRTCAR
                  out dx, al     ; Save Rn in CRTC Address Register
                  mov al, [cs:si]; lda      ,X+        ; Get Rn value from CRTC Table single + for +1, ++ is +2 and wrong!
                  mov dx, CRTCDR
                  out dx, al     ;  Save Rn data value in CRTC Data Register
                  inc si 
                  inc ah 
                  cmp ah, 0x10   ; Have we processed all register values                 
                  jne CRTCLOOP

                  mov dx, CRTC_DAC_PIXEL_MASK 
                  mov al, 0xFF   
                  out dx, al                 ; Pixel mask set so disabled

                  xor bx, bx
                  mov si, PALETTE
                  
                  mov cx, 32                 ; 32 Palette entries 16 CGA, 16 CGA Grey (4 repeated x 4)
NEXTPALETTE:      mov dx, CRTC_DAC_ADD_WR 
                  mov al, bl                  
                  out dx, al                 ; Output Palette address No.
                  mov dx, CRTC_DAC_COL_VALUE 
                  mov al, [cs:si]
                  inc si
				  nop
                  out dx, al                 ; Output Red 
				  nop
				  nop
                  mov al, [cs:si]
                  inc si
				  nop
                  out dx, al                 ; Output Green 
				  nop
				  nop
                  mov al, [cs:si]
                  inc si
				  nop
                  out dx, al                 ; Output Blue 
				  nop
				  nop
				  ; Read current address
                  mov dx, CRTC_DAC_ADD_RD 
                  mov al, bl                  
                  out dx, al                 ; Output Palette address No.
                  mov dx, CRTC_DAC_COL_VALUE 
				  nop
                  in  al,dx                  ; Input Red 
				  nop
				  nop
				  nop
                  in  al,dx                  ; Input Green
				  nop
				  nop
				  nop
                  in  al,dx                  ; Input Blue
				  nop
				  nop
                  inc bl
                  LOOP NEXTPALETTE

                 
                  ;int3
; Set Control Port to 80x25 Alpha Bit0=1, Graphics Bit1=0, B&W Bit2=0, Video Enable Bit3=1 and 640x200BW Bit4=0, Blink Bit5=0 
                  mov al, 0b00001001
                  mov dx, CGA_CONTROL
                  out dx, al
                  
CLRSCREEN:        xor di, di         ; ldx      #VRAM
                  mov ax, VRAMSEG
                  mov es, ax                  
                  mov al, ASCIISPACE ; lda      #ASCIISPACE
                  mov ah, CHARATTRIB ; ldb      #CHARCOLOUR
CLEARSCREEN1      mov [es:di], ax    ; stb      ,X+
                  add di, 0x02
                  cmp di, VRAMTOP    ; cmpx     #VRAMTOP
                  jbe CLEARSCREEN1   ; ble      CLEARSCREEN1
                  
sti
int3 ; Finish for Test 1 as we have no Video RAM or Character Generator yet


; ; For X the 8088 code will use es:di
; ; Character value is even byte (LSB al), Attribute is odd byte (MSB ah)                    
; TOPLINE:
    ; xor di, di         ; ldx      #VRAM
    ; mov ax, VRAMSEG
    ; mov es, ax                  
    ; mov al, 0x00       
    ; mov ah, CHARATTRIB ; ldb      #CHARCOLOUR

    ; mov al, 0x01       ; lda      #$01
; TOPLINE1:
    ; call PRINTDIGIT    ; TOPLINE1            lbsr      PRINTDIGIT
    ; inc al             ; adda      #$01
    ; daa                ; daa
    ; cmp di, VRAM + COLSPERROW * BYTESPERCHAR -1 ; cmpx      #VRAM + COLSPERROW * BYTESPERCHAR 
    ; jbe TOPLINE1       ; ble       TOPLINE1

; ; Print 10's under 1st line

; SECLINE:
    ; mov di, VRAM + COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR; SECLINE             ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
    ; mov al, 0x01       ; lda      #$1
; SECLINE1:    
    ; call PRINTDIGIT    ; SECLINE1            lbsr      PRINTDIGIT ; This will increase X by 2
    ; add di, 9*BYTESPERCHAR; leax     9*BYTESPERCHAR,X
    ; inc al             ; adda     #$01
    ; daa                ; daa
    ; cmp di, VRAM + 2 * COLSPERROW * BYTESPERCHAR -1 ; cmpx     #VRAM + 2 * COLSPERROW * BYTESPERCHAR ; 2 Bytes per character 0/even is character 1/odd is colour
    ; jbe SECLINE1       ; ble      SECLINE1


; LOWLINE:
    ; mov di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR ; LOWLINE             ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR 
    ; mov al, 0x01       ; lda      #$01
; LOWLINE1:
    ; call PRINTDIGIT    ; LOWLINE1            bsr      PRINTDIGIT
    ; inc al             ; adda     #$01
    ; daa                ; daa
    ; cmp di,VRAM + COLSPERROW * BYTESPERCHAR * ROWSPERPAGE -1 ; cmpx     #VRAM + COLSPERROW * BYTESPERCHAR * ROWSPERPAGE; 2 Bytes per character 0/even is character 1/odd is colour
    ; jbe LOWLINE1       ; ble      LOWLINE1

; LOWSECLINE:
    ; mov di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - 2 * COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR; LOWSECLINE          ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - 2 * COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
    ; mov al, 0x01       ; lda      #$1
; LOWSECLINE1:
   ; call PRINTDIGIT     ; LOWSECLINE1         bsr      PRINTDIGIT ; This will increase X by 2
   ; add di, 9*BYTESPERCHAR ; leax     9*BYTESPERCHAR,X
   ; inc al              ; adda     #$01
   ; daa                 ; daa
   ; cmp di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR - 1; cmpx     #VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR
   ; jbe LOWSECLINE1     ; ble      LOWSECLINE1

; LINENO:
    ; mov al, 0x02       ; LINENO              lda      #$02
    ; mov di, VRAM + COLSPERROW * BYTESPERCHAR ; ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR
; LINENO1:
    ; call PRINTNUM      ; LINENO1             bsr      PRINTNUM
    ; add di, 76*BYTESPERCHAR ; leax     76*BYTESPERCHAR,X  ; Move to end of line
    ; call PRINTNUM      ; bsr      PRINTNUM ; X should be start of next line after this
    ; inc al             ; adda     #$01
    ; daa                ; daa
    ; cmp al, ROWSPERPAGEBCD; cmpa     #ROWSPERPAGEBCD
    ; jne LINENO1        ; bne      LINENO1

   
; attribute:
; ; Set Control Port to B/W=1 Bit=1, Video Enable = 1 Bit = 3 and Blink=1 Bit = 5  00101010
    ; mov al, 0b00001000
    ; mov dx, CGA_CONTROL
    ; out dx, al
; ; Blink is enabled 1st time around    
; ATTRDUMP:
    ; xor ax, ax      ;
    ; ;mov al, ASCIISPACE ; Overwrite original character, Hex digits will show attribute    
    ; mov di, VRAM + 6 + 2 * COLSPERROW * BYTESPERCHAR  - 32 ; ldx       #VRAM - 32 + 7 + 2 * COLSPERROW * BYTESPERCHAR
; ATTRDUMP1:
    ; xor bx, bx      ; CHARDUMP1           clrb need to use ah for attributes
    ; add di, 16*BYTESPERCHAR ; leax      16*BYTESPERCHAR,X
; ATTRDUMP2:
    ; call PRINTHEX   ; CHARDUMP2           bsr       PRINTHEX
    ; mov [es:di], ax ; sta       ,X++
    ; add di, 0x2
    ; add di, 1*BYTESPERCHAR; leax      1*BYTESPERCHAR,X
    ; inc al
    ; inc ah          ; Next attribute
    ; inc bl          ; incb
    ; cmp bl, 0x10    ; cmpb      #$10
    ; jne ATTRDUMP2   ; bne       CHARDUMP2
    ; cmp ah, 0x00    ; tsta
    ; jne ATTRDUMP1   ; bne       CHARDUMP1


; COLDUMP:
    ; xor ax, ax      ;
    ; ;mov al, ASCIISPACE ; Overwrite original character, Hex digits will show attribute    
; COLDUMP1:
    ; xor bx, bx      ; CHARDUMP1           clrb need to use ah for attributes
    ; add di, 16*BYTESPERCHAR ; leax      16*BYTESPERCHAR,X
; COLDUMP2:
    ; mov ah, 0x0f
    ; mov al, bl
    ; call PRINTHEX   ; CHARDUMP2           bsr       PRINTHEX
    ; mov al, 0xDB
    ; mov ah, bl
    ; mov [es:di], ax ; sta       ,X++
    ; add di, 0x2
    ; add di, 1*BYTESPERCHAR; leax      1*BYTESPERCHAR,X
    ; inc bl          ; incb
    ; cmp bl, 0x10    ; cmpb      #$10
    ; jne COLDUMP2   ; bne       CHARDUMP2
   
    ; int3;
    
; ; ; Take BCD two digit number in al (A) and print at es:di (X) 
; PRINTNUM:                       ; PRINTNUM            pshs     a           ; Save A twice
    ; push ax                     ; pshs     a
    ; push ax
    ; and al, 0b11110000          ; anda     #%11110000  ; Mask LSB
    ; shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    ; shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    ; shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    ; shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    ; call PRINTDIGIT             ; bsr PRINTDIGIT
    ; pop ax                      ; puls     a           ; Restore A
    ; and al, 0b00001111          ; anda     #%00001111  ; Mask MSB
    ; call PRINTDIGIT             ; bsr PRINTDIGIT
    ; pop ax
    ; ret                         ; puls     a,pc

; ; ; Take Lower 4 bits in al (A) attribute in ah and print ASCII digit at es:di (X) and increment di by 2 (char & attribute)
; PRINTDIGIT:
    ; push ax                     ; PRINTDIGIT          pshs     a
    ; and al, 0b00001111          ; anda     #%00001111  ; Strip any MSB
    ; add al, ASCII0              ; adda     #ASCII0     ; Add 48 decimal, 0=$30, 1=$31
    ; mov [es:di], ax             ; sta      ,X++
    ; add di, 0x02
    ; pop ax
    ; ret                         ; puls     a,pc

; PRINTHEX:                       ;                  pshs     a            ; Save byte value as need to return since used for checksum calc
    ; push ax                     ;                  pshs     a            ; Save byte value as need to return since used for checksum calc
    ; push ax                     ;                  pshs     a            ; Save again so we can mask top and lower nibbles
    ; ; Must use al as daa only works with this register
    ; and al, 0b11110000          ;                  anda     #%11110000   ; Mask High nibble
    ; shr al,0x01                 ;                  lsra                  ; Shift to Low nibble
    ; shr al,0x01                 ;                  lsra 
    ; shr al,0x01                 ;                  lsra
    ; shr al,0x01                 ;                  lsra

    ; add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal 0-9  $30-$39 A-F $41-$46
    ; daa                         ;                  daa                    ; DAA on adding $90 sets carry which Makes 0-9 3x and A-F be 4x in adca
    ; adc al, 0x40                ;                  adca     #$40          ; Add $40 makes 0-9 D0-D9  in Decimal 130-139, A-F A0-A5 +$40 + Carry in decimal 141-146 = $41-$46
    ; daa                         ;                  daa                    ; Strips 100 from result to give 30-39 and 41-46

    ; mov [es:di], ax             ; sta      ,X++
    ; add di, 0x02

    ; pop ax                      ;                  puls     a 
    ; and al, 0b00001111          ;                  anda     #%00001111    ; Mask Low nibble

    ; add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal, same as above
    ; daa                         ;                  daa
    ; adc al, 0x40                ;                  adca     #$40
    ; daa                         ;                  daa
    ; ; mov ah, al now using al for A

    ; mov [es:di], ax             ; sta      ,X++
    ; add di, 0x02
    ; pop ax                      ;                  puls     a
    ; ret                         ;                  rts

  
  
CRTCTAB             db      0x63         ; R0 H 62 to 64 Total 99
                    db      0x50         ; R1 H Displayed 80
                    db      0x54         ; R2 H from x53 to x55 Sync Position 83 0x53
                    db      0x0C         ; R3 H Sync Width 12/0C
                    db      0x1B         ; R4 V Total 27
                    db      0x01         ; R5 V Total Adjust (was 03 then 13/$0D)
                    db      0x19         ; R6 V Displayed 25
                    db      0x19         ; R7 V Sync Position 26 (Was 1A)
                    db      0x50         ; R8 Interlace mode - Non Interlaced, DISPEN SKEW 1 Char 10, CUR SKEW 1 char 40
                    db      0x0F         ; R9 Maximum Scan Line Address 
                    db      0x0d         ; R10 Cursor Start - No Blink + Line 13 Start  Cursor off $0x20
                    db      0x0f         ; R11 Cursor End - No Blink + Line 15 Finish (Blinking coordinated with char blink by PLD)
                    db      0x00,0x00    ; R12,R13 Start Address
                    db      0x00,0x00    ; R14,R15 Cursor Address

; Palette colors are 0-63 not 0-255 so FF=3F, AA=2A, 55=15
; First 16 are CGA, then 4 MDA with green, 4 MDA with White   P4=0 CGA, P4=1 MDA, P2=0 Green P2=1 White
; Just for for character generator testing need black & White
;  Colour for Monochrome Green R, G, B
; 00  0x0, 0x0, 0x0  Black/Background
; 01  0x0, 0x2A, 0x0 Green Foreground 66%
; 10  0x0, 0x15, 0x0 Bright Background 33%
; 11  0x0, 0x3f, 0x0 Bright Green Foreground 100% 0-63d, 6 bits per colour

;  Colour for Monochrome White R, G, B
; 00  0x0, 0x0, 0x0  Black/Background
; 01  0x2A, 0x2A, 0x2A White Foreground 66%
; 10  0x15, 0x15, 0x15 Bright Background 33%
; 11  0x3f, 0x3f, 0x3f Bright White Foreground 100% 0-63d, 6 bits per colour

; For CGA Ignore MDA Palettes and use 17-31 for greyscale for when B&W Mode enabled to simulate Composite output without chromiance

PALETTE             db      0x00, 0x00, 0x00  ; Black (#000000)
;                    db      0x3F, 0x3F, 0x3F  ; White (#FFFFFF) Comment this in for testing using P0 for fore/back, comment out blue
                    db      0x00, 0x00, 0x31  ; Blue (#0000C4)
                    db      0x00, 0x31, 0x00  ; Green (#00C400)
                    db      0x00, 0x31, 0x31  ; Cyan (#00C4C4)
                    db      0x31, 0x00, 0x00  ; Red (#C40000)
                    db      0x31, 0x00, 0x31  ; Magenta (#C400C4)
                    db      0x31, 0x1F, 0x00  ; Brown (#C47E00)
                    db      0x31, 0x31, 0x31  ; Light Gray (#C4C4C4)
                    db      0x13, 0x13, 0x13  ; Dark Gray (#4E4E4E)
                    db      0x13, 0x13, 0x31  ; Light Blue (#4E4EDC)
                    db      0x13, 0x37, 0x13  ; Light Green (#4EDC4E)
                    db      0x13, 0x3c, 0x3c  ; Light Cyan (#4EF3F3)
                    db      0x37, 0x13, 0x13  ; Light Red (#DC4E4E)
                    db      0x3c, 0x13, 0x3c  ; Light Magenta (#F34EF3)
                    db      0x3c, 0x3c, 0x13  ; Yellow  (#F3F34E)
                    db      0x3F, 0x3F, 0x3F  ; White (#FFFFFF)

                                              ; Order of greyscale values may need adjusting
                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground
