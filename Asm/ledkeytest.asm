;=========================================================================
; ledkeytest.asm - Test Breadboard PC 7 Segment Display and Key Pad
; Note this is a literal translation of the 6802/6809 code and is not efficient or best practice!
; The Nanocomp 6809 monitor was a quick translation of the 6802 version in the first place!
; Based on XI 8088 Bios code by Sergey Kiselev
; nasm -f bin -o ledkeytest.bin -l ledkeytest.lst ledkeytest.asm
;
; Displays Nanocomp then waits for a key to be pressed and displays key code
;=========================================================================

cpu	8086

; This macro includes the setloc macro to pad code with FF to a given address
%include "macro.inc"
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
segM                          EQU 0x6E ; (upside down U)
segN                          EQU 0x6E ; (upside down U)
segO                          EQU 0x7E
segP                          EQU 0x6B
segS                          EQU 0x3D ; (Same as 5)
segU                          EQU 0x76
segX                          EQU 0x67
segY                          EQU 0x37
segDash                       EQU 0x01
segSpace                      EQU 0x00

keyCN                         EQU 0x25
keyG                          EQU 0x35
keyI                          EQU 0x32
keyL                          EQU 0x05
keyM                          EQU 0x31
keyP                          EQU 0x15 ; P Punch replaced with S Save
keyS                          EQU 0x15
keyR                          EQU 0x30
key0                          EQU 0x22
key1                          EQU 0x24
key2                          EQU 0x02
key3                          EQU 0x12
key4                          EQU 0x14
key5                          EQU 0x00
key6                          EQU 0x10
key7                          EQU 0x04
key8                          EQU 0x01
key9                          EQU 0x11
keyA                          EQU 0x03
keyB                          EQU 0x13
keyC                          EQU 0x23
keyD                          EQU 0x33
keyE                          EQU 0x21
keyF                          EQU 0x20



%define	START		0x8000		; BIOS/ROM starts at offset 8000h Physical Address is F8000 [F000:8000]
%define	DBUF		0xFFF0		; Display Buffer DBUF starts at offset FFF0 Physical Address is 7FFF0 [7000:FFF0] at top of 512K RAM
; Note es set to D000 for PIA
%define	PIASEG  0xD000		; PIA Segment A D0000    [D000:0000]

%define	PORTA		0x0000		; PIA Port A D0000    [D000:0000]
%define	CTRLA		0x0001		; PIA Control A D0001 [D000:0001]
%define	PORTB		0x0002		; PIA Port B D0002    [D000:0002]
%define	CTRLB		0x0003		; PIA Control B D0003 [D000:0003]
    

org	START		; Use only upper 32 KiB of ROM

test_start:
    ; Set up the segment registers
    mov ax, 0x7000
    mov ss, ax     ; Set Stack Segment at 0x7000
    mov ds, ax     ; Set ds to 0x7000
    mov ax, PIASEG 
    mov es, ax     ; Set es to 0xD000 for PIA
    mov sp, 0xFF00 ; Stack starts at 0x7FF00 downwards [7000:FF00] leaving 256 bytes for Monitor working memory
    xor di, di
    xor si, si
    mov ax, 0
    mov bx, 0

fill_disp_buff:
    ; Write NAN0C0MP
    mov di, DBUF
    mov byte [ds:di], segN
    inc di
    mov byte [ds:di], segA
    inc di
    mov byte [ds:di], segN
    inc di
    mov byte [ds:di], segO
    inc di
    mov byte [ds:di], segC
    inc di
    mov byte [ds:di], segO
    inc di
    mov byte [ds:di], segM
    inc di
    mov byte [ds:di], segP
    ; Test r7seg 0-7
    call dispresh
    call getkey
    mov di, DBUF
    mov ah, 0x00
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x01
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x02
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x03
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x04
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x05
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x06
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x07
    call r7seg
    mov byte [ds:di], ah
    ; Test r7seg 8-f
    call dispresh
    call getkey
    mov di, DBUF
    mov ah, 0x08
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x09
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0a
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0b
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0c
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0d
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0e
    call r7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x0f
    call r7seg
    mov byte [ds:di], ah

    ; Test l7seg 00-70
    call dispresh
    call getkey
    mov di, DBUF
    mov ah, 0x00
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x10
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x20
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x30
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x40
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x50
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x60
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x70
    call l7seg
    mov byte [ds:di], ah
    ; Test l7seg 80-f0
    call dispresh
    call getkey
    mov di, DBUF
    mov ah, 0x80
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0x90
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xa0
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xb0
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xc0
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xd0
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xe0
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah, 0xf0
    call l7seg
    mov byte [ds:di], ah

    call dispresh
    call getkey
    mov di, DBUF
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace
    inc di
    mov byte [ds:di], segSpace

new_key:
    call getkey
    mov bl,ah   ; Save ah to bl
    mov di, DBUF
    call l7seg
    mov byte [ds:di], ah
    inc di
    mov ah,bl   ; copy back ah
    call r7seg
    mov byte [ds:di], ah
    jmp new_key
    
for_ever:    
    jmp for_ever

    ; Note es should be set to PIA map D000, ds to 7000    
dispresh:	
    push ax	           ;dispresh             pshs     x,b,a
    push di            ; di=x as destination
    push si            ; si=x as source
                       ; ah=a al=b (6809 D=AB)
	  ;   set output ports for display using es segment (will be replaced by IO ports in future)
    mov di, PORTA	     ;                     ldx      #PORTA            ; This routine uses a different approach from GETKEY using index X and offsets
    xor ax,ax	         ;                     clra                       ; note 6809 offsets changed for RS0=A0 & RS1=A1
    mov [es:di+1], ah	 ;                     sta      $01,x             ; Set CTRLA Port A to data direction register b2=0 DDR
    mov [es:di+3], ah	 ;                     sta      $03,x             ; Set CTRLB Port B to data direction register
    mov ax, 0x7f       ;                     lda      #$7F              ; 
    mov [es:di], ax	   ;                     sta      ,x                ; Set Port A to output bits 0-6
    mov ah, 0x0f       ;                     lda      #$0F              ; Set bits 0-3 for output
    mov [es:di+2], ah	 ;                     sta      $02,x             ; Set Port B to output for bits 0-3
    mov ah, 0x04       ;                     lda      #$04              ; Set control register b2=1 Output Register
    mov [es:di+1],ah	 ;                     sta      $01,x             ; Set CTRLA Port A control register for Port Output, will be keypad switch input
    mov [es:di+3],ah	 ;                     sta      $03,x             ; Set CTRLB Port B control register for Port Output, will be keypad row
	
    ; initialise loop over digits
    mov si, DBUF	    ;                      ldx      #dbuf             ; Segments are numbered 4..9 to correspond to outputs from the 74LS145/7442 decoder.
    mov al, 0x03      ;	                     ldb      #$03
disp1:
    inc al	          ;disp1                 incb                       ; Start at 04
    cmp al, 0x0c      ;	                     cmpb     #$0C              ; until done 11/0B
    jne disp2 	      ;                      bne      DISP2             ; Display next digit
	
	  ; finished
    pop si
    pop di	          ;                      puls     a,b,x,pc          ;(pul? pc=rts) Done all 6 segments 04-09
    pop ax	
    ret	

    ; light up the next digit                     ; Changes to reduce Ghosting of 7 Segment displays
disp2:
    mov ah, 0x7f      ;disp2                lda      #$7F              ; This turns off all 7 Segments
	  mov [es:di], ah   ;                     sta      PORTA             ; Turn off current segments before changing PORTB
	  mov ah, [ds:si]   ;                     lda      ,x+               ; Get segment value from Display buffer first
    inc si
    not ah	          ;                     coma                       ; Complement 7 Segment values as 1 turns off segment
	  mov [es:di+2], al ;                     stb      PORTB             ; Select segment on Port B (04-09)            
	  mov [es:di], ah   ;                     sta      PORTA               ; Save segment bits to Port A
	
	  ;   delay loop
    mov ah, 0xa0	    ;                     lda      #$A0              ; Delay loop for 160 iterations
disp3:
    dec ah            ;disp3                deca                       
    jne disp3	        ;                     bne      DISP3             ; Delay until A is zero
    jmp disp1	        ;                     bra      DISP1             ; Process next segment


; Key codes
;  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  L SP CN G  M  R  I
; 22 24 02 12 14 00 10 04 01 11 03 13 23 33 21 20 05 15 25 35 31 30 32

; Uses a=ah, b=al x=di y=si returns key code in ah

getkey:
    push bx           ;GETKEY               pshs     y,dp,b
                      ;                     lda      #dpPIA            ; Set Direct Page register to PIA page ($40), PIA code uses Direct addressing 
                      ;                     tfr      a,dp

;   Set up I/O port for Port A 0-7 input and Port B 0-3 output
getkey1:
    call dispresh     ;GETKEY1              bsr      DISPRESH          ; Refresh 7 Segment display while waiting for key
    xor ax, ax        ;                     clra     
    mov [es:CTRLA], ah;                     sta      <CTRLA            ; Set Port A to data direction register b2=0 DDR
    mov [es:CTRLB], ah;                     sta      <CTRLB            ; Set Port B to data direction register
    mov [es:PORTA], ah;                     sta      <PORTA            ; Set Port A to input bits 0-7
    mov ah, 0x0f      ;                     lda      #$0F              ; set bits 0-3 for output
    mov [es:PORTB], ah;                     sta      <PORTB            ; Set Port B to output for bits 0-3
    mov ah, 0x04      ;                     lda      #$04              ; Set control register b2=1 Output Register
    mov [es:CTRLA], ah;                     sta      <CTRLA            ; Set Port A control register for Port Output, will be keypad switch input
    mov [es:CTRLB], ah;                     sta      <CTRLB            ; Set Port B control register for Port Output, will be keypad row
    mov al, 0xff      ;                     lda(b)      #$FF           ; *** This should probably set ldb to $FF, bug in original Monitor Code
getkey2:              ; B register is used to strobe keypad output PB0, PB1 to drive 74LS145 decoder (7442 in original)
    inc al            ;GETKEY2              incb                       ; Read next keypad row (row will change to 0, key pressed changes from 1 to 0)
    cmp al,0x04       ;                     cmpb     #$04              ; Done rows 0,1,2,3?
    je getkey1        ;                     beq      GETKEY1           ; Scanned all 4 keypad lines, update display and start again
    mov [es:PORTB], al;                     stb      <PORTB            ; Output current keypad row to Port B, selected row output will be set to 0 (default to 1)
    mov ah, [es:PORTA];                     lda      <PORTA            ; Read keypad keys from Port A (note Port A is pulled high, pressed key sets bit to 0)
    not ah            ;                     coma                       ; Complement keypad input so pressed key is indicated by 1
    cmp ah, 0x00
    je getkey2        ;                     beq      GETKEY2           ; If all zero, no key pressed, try next keypad row

;   Found a key: decode it, use bh for tmp3 (PA0-5) and bl for tmp2 (Q0-3)
    mov bl, al        ;                     stb      tmp2              ; Save the keypad row in B to tmp2 (better name ****)
    mov bh, ah        ;                     sta      tmp3              ; Save input key to tmp3 (was chksum which maps to same address $13ed)
    xor ah,ah         ;                     clra                       ; A maintains the bit count of which bit is set
    mov al, 0x01      ;                     ldb      #$01              ; Set b0 as test bit
getkey3:
    cmp al,bh         ;GETKEY3              cmpb     tmp3              ; Is bit A set?, what happens if two keys pressed, just the lowest?
    je getkey4        ;                     beq      GETKEY4           ; If bit is set calculate key code GETKEY4
    inc ah            ;                     inca                       ; Next bit count                       
    shl al,0x01       ;                     aslb                       ; Rotate bit to next positon, fill b0 with 0, when all 0 set Z (eq)
    cmp al,0x00
    je getkey2        ;                     beq      GETKEY2           ; When done 0-7 then process next keypad row
    jmp getkey3       ;                     bra      GETKEY3           ; Check next bit

getkey4:
    mov al,bl         ;GETKEY4              ldb      tmp2              ; Get the keypad row (Q0-3)
    shl al,0x01       ;                     aslb                       ; Shift to the top nibble
    shl al,0x01       ;                     aslb     
    shl al,0x01       ;                     aslb     
    shl al,0x01       ;                     aslb     
    add ah,al         ;                     pshs     b                 ; Add B to A via stack
                      ;                     adda     ,s+               ; Add value of B on stack to A to calculate keycode

;   Wait for the key to be released
    push ax           ;                     pshs     a                 ; Save keycode
                      ; Use bx for Y and B
    mov bx, 0x0800    ;                     ldy      #$08              ; Keybounce delay will be 8 (Y) x 256 (B), 2048 loops
getkey5:
                      ;GETKEY5              clrb                       ; Initialise B as inner loop counter
getkey6:                      
    mov ah, [es:PORTA];GETKEY6              lda      <PORTA            ; Check port to see if key still pressed (bit=0 key pressed)
    not ah            ;                     coma                       ; Complement A, non zero (NE) means a key still held down
    cmp ah,0x00
    jne getkey5       ;                     bne      GETKEY5           ; If key still pressed, zero count and check again 
    dec bx            ;                     decb                       ; Key released, decrement count
    jne getkey6       ;                     bne      GETKEY6           ; If still not zero check port again (this will de-bounce keys)
                      ;                     leay     -$01,y            ; Counted down B to 0 from 255, decrement Y
                      ;                     bne      GETKEY6           ; Check port again, until Y is zero
    pop ax            ;                     puls     a,b,dp,y,pc       ; (pul? pc=rts) Keycode returned in A 
    pop bx
    ret

; 7CE4 KEYHEX Combines GETKEY and HEXCON
; Gets a single key digit 0-F, converts to 7 Segment, displays at X then returns hex value, return to monitor if invalid key
keyhex:
    call getkey       ;KEYHEX               lbsr     GETKEY            ; Get a single key

;   Fall-thru
; 7CE7 HEXCON converts key code in A into Hex equivalent for the key and returns in A. 
; If non Hex command key entered returns to monitor
hexcon:
    push bx          ;HEXCON                pshs     x,b
    mov bx,KEYCODE-1 ;                      ldx      #KEYCODE          ; Lookup key in KEYCODE table
    mov al, 0xff     ;                      ldb      #$FF
hexcon1:
    inc al           ;HEXCON1               incb                       ; B will have hex value of KEYCODE
    inc bx
    cmp al,0x10      ;                      cmpx     #SVNSEG           ; Check if X is at end of KEYCODE table (or al < 0x10)
    je for_ever      ;                      beq      TORESUME2         ; If it is at end without finding key, the resume to monitor **** (may not need toresume2 perhaps to avoid lbeq)
    cmp ah, [cs:bx]  ;                      cmpa     ,x+               ; Compare KEYCODE at X with current key value
    jne hexcon1      ;                      bne      HEXCON1           ; If no match then move to next KEYCODE value
    mov ah,al        ;                      tfr      b,a               ; Hex digit value returned in A
    pop bx           ;                      puls     b,x,pc ;(pul? pc=rts)
    ret 
    
; 7CFF L7SEG converts left hex digit of ah into 7 segment code for display and returns in ah
l7seg:
    ; we could use cl=4
    shr ah,0x01       ;L7SEG                asra                       ; Shift MSB right 4 bits so it is LSB
    shr ah,0x01       ;                     asra     
    shr ah,0x01       ;                     asra     
    shr ah,0x01       ;                     asra     

; 7D03 R7SEG converts right hex digit of ah into 7 segment code for display and returns in ah
;   Convert low-order 4 bits of ah to 7seg
r7seg:
    push si           ;R7SEG                pshs     x                 ; Save X, used as index to lookup 7 segment values
    push bx
    mov bx, SVNSEG    ;                     ldx      #SVNSEG           ; X is 7 segment lookup table start
    and ah, 0x0f      ;                     anda     #$0F              ; mask MSB
    mov al, ah
    xor ah, ah
    mov si, ax        ;R71                  beq      R72               ; A=0 we have found value already
                      ;                     leax     $01,x             ; Increment X
                      ;                     deca                       ; Decrease A (note that leax A,X would avoid the need for the loop)
                      ;                     bra      R71               ; Check next value
    mov ah, [cs:bx+si];R72                  lda      ,x                ; Get 7 segment value for A
    pop bx            ;puls     x,pc              ;(pul? pc=rts) return A and restore X 
    pop si
    ret
    
; Keypad keycode lookup table to convert key to nibble values, used by KEYHEX and HEXCON
KEYCODE:             db  key0, key1, key2, key3, key4, key5, key6, key7
                     db  key8, key9, keyA, keyB, keyC, keyD, keyE, keyF 

; Seven Segment display lookup table, used to convert 7 Seg value to nibble and nibble to 7 Seg value
; Used by HEXCON, R7SEG and SVNHEX subroutines
SVNSEG:              db  seg0, seg1, seg2, seg3, seg4, seg5, seg6, seg7
                     db  seg8, seg9, segA, segB, segC, segD, segE, segF


;=========================================================================
; start - at power up or reset execution starts here (F000:FFF0)
;-------------------------------------------------------------------------
        setloc	0FFF0h			; Power-On Entry Point, macro fills space from last line with FF
start:
        jmp     0F000h:test_start
        setloc	0FFFFh			; Pad remainder of ROM
	      db	0ffh
