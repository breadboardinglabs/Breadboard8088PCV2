;=========================================================================
; ledkeytestv3.asm - Test Breadboard PC 7 Segment Display and NeoKey Pad
; Based on Reset code and macro from XI 8088 BIOS code by Sergey Kiselev
; nasm -f bin -o ledkeytestv3.bin -l ledkeytestv3.lst ledkeytestv3.asm
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

; Key codes updated for NeoKey Keypad
keyCN                         EQU 0x02
keyG                          EQU 0x03
keyI                          EQU 0x44
keyL                          EQU 0x00
keyM                          EQU 0x24
keyP                          EQU 0x01 ; P Punch replaced with S Save
keyS                          EQU 0x01
keyR                          EQU 0x34
key0                          EQU 0x43
key1                          EQU 0x40
key2                          EQU 0x41
key3                          EQU 0x42
key4                          EQU 0x30
key5                          EQU 0x31
key6                          EQU 0x32
key7                          EQU 0x20
key8                          EQU 0x21
key9                          EQU 0x22
keyA                          EQU 0x10
keyB                          EQU 0x11
keyC                          EQU 0x12
keyD                          EQU 0x13
keyE                          EQU 0x23
keyF                          EQU 0x33
keyCS                         EQU 0x04
keyES                         EQU 0x14

%define	START		0x8000		; BIOS/ROM starts at offset 8000h Physical Address is F8000 [F000:8000]
%define	DBUF		0xFFF0		; Display Buffer DBUF starts at offset FFF0 Physical Address is 7FFF0 [7000:FFF0] at top of 512K RAM

%define	PORTA		0x0400		; Equivalent of PIA Port A D0000
%define	PORTB		0x0401		; Equivalent of PIA Port B D0002
    

org	START		; Use only upper 32 KiB of ROM

test_start:
    ; Set up the segment registers
    mov ax, 0x7000
    mov ss, ax     ; Set Stack Segment at 0x7000
    mov ds, ax     ; Set ds to 0x7000
    mov sp, 0xFF00 ; Stack starts at 0x7FF00 downwards [7000:FF00] leaving 256 bytes for Monitor working memory
    xor di, di
    xor si, si
    mov ax, 0
    mov bx, 0

fill_disp_buff:
    ; Write NANOCOMP
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
test1:
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
   ; Waits for key, calling Dispresh while doing so, will display key code pressed
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

    ; Note DS set to 7000    
dispresh:	
    push ax	           ; Save registers used
	push bx
    push dx            ; dx as IO Port
    push si            ; si=x as source

	;   set IO output ports for display using dx for OUT and IN
    mov dx, PORTA	     ;
    xor ax,ax	         ;
	
    ; initialise loop over digits, since need AL for OUT/IN need to swap with AH compared with original ledkeytest.asm
    mov si, DBUF	    ;                      ldx      #dbuf             ; Segments are numbered 4..9 to correspond to outputs from the 74LS145/7442 decoder.
    mov ah, 0x04        ; Offset to LED Segemnts Now 0x04 for ROW1-5 not Q0-Q3 was 0x03 Port B value 05=Q0 used for LED segment 1
disp1:
    inc ah	          ; now start at 0x05
    cmp ah, 0x0d      ; until done Port B =13/0x0D
    jne disp2 	      ; Display next digit
	
	  ; finished
    pop si
    pop dx          ;
	pop bx
    pop ax	
    ret	

    ; light up the next digit                     ; Changes to reduce Ghosting of 7 Segment displays
disp2:
    mov al, 0xff      ; This value turns off all 7 Segments and DP
	out dx, al        ; Turn off current segments before changing PORTB
	mov al, [ds:si]   ; Get segment value from Display buffer first
    inc si
    not al	          ; Complement 7 Segment values as 1 turns off segment
	mov dx, PORTB     ; Would probably be easier to inc and dec dx between PORTA and PORTB but this is more obvious
	mov bl, al        ; Save al to bl before IO OUT
	mov al, ah
	out dx, al        ; Select segment on Port B (05-12/0x0c)
	mov dx, PORTA
	mov al,bl         ; restore al from bl to send 7 segment bits to PORTA
	out dx, al        ; Save segment bits to Port A
	
	  ;   delay loop
    mov al, 0xa0	    ;                     lda      #$A0              ; Delay loop for 160 iterations
disp3:
    dec al            ;disp3                deca                       
    jne disp3	        ;                     bne      DISP3             ; Delay until A is zero
    jmp disp1	        ;                     bra      DISP1             ; Process next segment


; Key codes for NeoKey keypad
;  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  L SP CN  G  M  R  I
; 43 40 41 42 30 31 32 20 21 22 10 11 12 13 23 33 00 01 02 03 24 34 44
; Uses a=ah, b=al x=di y=si returns key code in ah

getkey:
    push bx
	push dx

;   Set up I/O port for Port A 0-7 input and Port B 0-3 output, for IO Ports need to swap use of al and ah
getkey1:
    call dispresh     ;GETKEY1              bsr      DISPRESH          ; Refresh 7 Segment display while waiting for key
    xor ax, ax        ;                     clra     

    mov al, 0xff      ;                     lda(b)      #$FF           ; *** This should probably set ldb to $FF, bug in original Monitor Code
getkey2:              ; B register is used to strobe keypad output PB0, PB1 to drive 74LS145 decoder (7442 in original)
    inc al            ; Read next keypad row (row will change to 1, key pressed changes from 0 to 1)
    cmp al,0x05       ; Done rows 0,1,2,3,4 ROW1-ROW5?
    je getkey1        ; Scanned all 5 keypad row lines, update display and start again
	mov dx, PORTB
    out dx, al        ; Output current keypad row to Port B, selected row output will be set to 1 as NeoKey needs 5V for ROW
	mov dx, PORTA
	mov bl,al         ; Save al to bl
	mov al, ah
    in al,dx          ; Read keypad keys from Port A (note Port A is pulled high, pressed key sets bit to 0)
	mov ah,al         ; set ah to value from PORTA
	mov al,bl         ; Restore al from bl
    ;                  Don't need to Complement keypad input now as pressed key is indicated by 1
    cmp ah, 0x00
    je getkey2        ;  If all zero, no key pressed, try next keypad row

;   Found a key: decode it, use bh for tmp3 (PA0-4) and bl for tmp2 (PB0-4)
    mov bl, al        ; Save the keypad row in bl
    mov bh, ah        ; Save input key to bh
    xor ah,ah         ; ah maintains the bit count of which bit is set
    mov al, 0x01      ; Set b0 as test bit
getkey3:
    cmp al,bh         ; Is bit al set?, what happens if two keys pressed, just the lowest?
    je getkey4        ; If bit is set calculate key code GETKEY4
    inc ah            ; Next bit count                       
    shl al,0x01       ; Rotate bit to next positon, fill b0 with 0, when all 0 set Z (eq)
    cmp al,0x00
    je getkey2        ; When done 0-7 then process next keypad row
    jmp getkey3       ; Check next bit

getkey4:
    mov al,bl         ; Get the keypad row (PB0-4)
    shl al,0x01       ; Shift to the top nibble
    shl al,0x01       ;
    shl al,0x01       ;
    shl al,0x01       ;
    add ah,al         ; Add ah to al to leave keycode in ah

;   Wait for the key to be released
    push ax           ; Save keycode in ah
                      ; Use bx for Y and B
    mov bx, 0x0800    ;                     ldy      #$08              ; Keybounce delay will be 8 (Y) x 256 (B), 2048 loops
getkey5:
                      ;GETKEY5              clrb                       ; Initialise B as inner loop counter
getkey6:
    mov dx, PORTA                      
    in al, dx         ; Check port to see if key still pressed (bit=1 key pressed)
                      ; don't need to complement al, NeoKey returns 1 for key pressed
    cmp al,0x00
    jne getkey5       ; If key still pressed, zero count and check again 
    dec bx            ; Key released, decrement count
    jne getkey6       ; If still not zero check port again (this will de-bounce keys)

    pop ax            ; Restores key code in ah 
    pop dx
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
