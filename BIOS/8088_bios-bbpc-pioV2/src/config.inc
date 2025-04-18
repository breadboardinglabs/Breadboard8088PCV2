;=========================================================================
; config.inc - Compilation time settings and settings
;-------------------------------------------------------------------------
;
; Compiles with NASM 2.13.02, might work with other versions
;
; Copyright (C) 2010 - 2023 Sergey Kiselev.
; Provided for hobbyist use on the Xi 8088 and Micro 8088 boards.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================

%define DATE		'09/29/23'	; BIOS release date MM/DD/YY
%define VERSION		'1.0.0'		; BIOS version

; Machine type is defined in the Makefile
;%define MACHINE_XI8088			; Xi 8088
;%define MACHINE_FE2010A 		; Faraday FE2010A
;%define MACHINE_BOOK8088		; Book8088
;%define MACHINE_HOMEBREW8088		; EMM Computers "Homebrew8088" board
;%define MACHINE_XT			; IBM PC/XT or highly compatible board
;%define MACHINE_BBPC8088	        ; Breadboard PC 8088

					; FIXME: not implemented yet

; Settings for Xi 8088
%ifdef MACHINE_XI8088
%define	START		8000h		; BIOS starts at offset 8000h
%define MODEL_BYTE	0FCh		; Pretend it is an IBM PC/AT
%define AT_RTC				; Has AT-compatible RTC
%define AT_RTC_NVRAM			; Use AT RTC for BIOS configuration
%define AT_NMI				; Use port 70h for NMI enable/disable
%define AT_DELAY			; Use refresh bit of port 61h for delays
%define SECOND_PIC			; Has two PICs (AT-style)
%define AT_KEYBOARD			; Has AT-compatible keyboard controller
%define PS2_MOUSE			; Enable PS/2 auxiliary device support
%define DISABLE_KBD_DURING_INTERRUPTS	; Don't disable keyboard in INT1
%define MIN_RAM_SIZE	32		; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		; Scan this much memory during POST
%define RAM_TEST_BLOCK	16384		; block size for RAM test
%define EBDA_SIZE	1		; 1KB reserved for EBDA
					; EBDA is required for PS/2 aux support
%endif ; MACHINE_XI8088

; Settings for FE2010A
%ifdef MACHINE_FE2010A
%define	START		0A000h		; BIOS starts at offset 0A000h
%define MODEL_BYTE	0FEh		; IBM PC/XT
%define AT_RTC				; Has AT-compatible RTC
%define AT_RTC_AUTODETECT		; Autodetect RTC presence
%define FLASH_NVRAM			; Use Flash ROM for BIOS configuration
%define MIN_RAM_SIZE	32		; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		; Scan this much memory during POST
%define RAM_TEST_BLOCK	16384		; block size for RAM test
%define PIT_DELAY			; Use PIT polling for delays
%endif ; MACHINE_FE2010A

; Settings for Book8088
%ifdef MACHINE_BOOK8088
%define	START		0C000h		; BIOS starts at 0C000h - 16 KiB total
%define MODEL_BYTE	0FEh		; IBM PC/XT
%define MIN_RAM_SIZE	32		; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		; Scan this much memory during POST
%define RAM_TEST_BLOCK	16384		; block size for RAM test
%define PIT_DELAY			; Use PIT polling for delays
%endif ; MACHINE_BOOK8088

; Settings for IBM PC/XT
%ifdef MACHINE_XT
;%define	START		0E000h		; BIOS starts at offset 0E000h
%define	START		0C000h		; FIXME: more ROM for development
%define MODEL_BYTE	0FEh		; IBM PC/XT
%define MIN_RAM_SIZE	32		; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		; Scan this much memory during POST
%define RAM_TEST_BLOCK	16384		; block size for RAM test
%define PIT_DELAY			; Use PIT polling for delays
%endif ; MACHINE_XT

%ifdef MACHINE_HOMEBREW8088
%define	START		0C000h		; FIXME: more ROM for development
%define MODEL_BYTE	0FEh		; IBM PC/XT
%define MIN_RAM_SIZE	32		; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		; Scan this much memory during POST
%define RAM_TEST_BLOCK	16384		; block size for RAM test
%define PIT_DELAY			; Use PIT polling for delays
%define AT_KEYBOARD			; Has AT-compatible keyboard controller
%define DISABLE_KBD_DURING_INTERRUPTS
; Optional: Enable DS12885/DS12887 based RTC
;%define AT_RTC				; Has AT-compatible RTC
;%define AT_RTC_AUTODETECT		; Autodetect RTC presence
; Specify RTC I/O address if different from the default 0x70
;%define AT_RTC_PORT	2A0h
%endif ; MACHINE_HOMEBREW8088

; Settings for Breadboard PC 8088
%ifdef MACHINE_BBPC8088
%define	START		9000h		; BIOS starts at offset 8000h by default but 9000 leaves 4K for XTIDE Tiny BIOS
;%define	START		8000h		     ; BIOS starts at offset 8000h
;%define MODEL_BYTE	0FEh		; IBM PC/XT - no key lock!
;%define MODEL_BYTE	0F8h		; IBM PS/2 Model 80 (Sub Model 0 by default is 16 MHz Model 80) with Keyboard/Aux on 60 & 64 needed for MS MOUSE Driver to see PS/2 Mouse (model 25&30 are odd)! Could also try FC AT and Sub 4 Model 50 Sub 5 Model 60
;%define MODEL_SUB_BYTE  000h            ; (PS/2 Model 80 F8 Sub Model 0 by default is 16 MHz Model 80) Model FC PC/AT and Sub 0 or  Sub 4 PS/2 Model 50 Sub 5 PS/2 Model 60
%define MODEL_BYTE	0FCh		; IBM PC-AT Sub 4 Model 50 Sub 5 Model 60
%define MODEL_SUB_BYTE  004h            ; (PS/2 Model 80 F8 Sub Model 0 by default is 16 MHz Model 80) Model FC PC/AT and Sub 0 or  Sub 4 PS/2 Model 50 Sub 5 PS/2 Model 60
;%define MODEL_BYTE	0FCh		; Pretend it is an IBM PC/AT but this may enable key lock stuff disabling mouse!
%define AT_RTC				; Has AT-compatible RTC
%define AT_RTC_NVRAM			; Use AT RTC for BIOS configuration
%define AT_DELAY			; Use refresh bit of port 61h for delays
%define AT_KEYBOARD			; Has AT-compatible keyboard controller
%define SECOND_PIC			; Has two PICs (AT-style)
%define PS2_MOUSE			       ; Enable PS/2 auxiliary device support
%define DISABLE_KBD_DURING_INTERRUPTS	; Don't disable keyboard in INT1
%define MIN_RAM_SIZE	32		 ; At least 32 KiB to boot the system
%define MAX_RAM_SIZE	640		 ; Scan this much memory during POST
%define RAM_TEST_BLOCK 16384 ; block size for RAM test
;%define PIT_DELAY			; Use PIT polling for delays
%define EBDA_SIZE	1		; 1KB reserved for EBDA
					; EBDA is required for PS/2 aux support
%endif ; MACHINE_BBPC8088

; Automatic settings based on the machine settings above
%ifdef AT_RTC or AT_RTC_NVRAM or FLASH_NVRAM
%define BIOS_SETUP			; Include BIOS setup utility
%endif ; AT_RTC or AT_RTC_NVRAM or FLASH_NVRAM

; Note: while Book8088 does have a turbo mode, it is not software controlled
%ifdef MACHINE_XI8088 or MACHINE_FE2010A
%define TURBO_MODE			; Has turbo mode
%endif ; MACHINE_XI8088 or MACHINE_FE2010A

; Default floppy drives configuration for systems without NVRAM/Flash ROM setup
; First digit = drive A, second = drive B
; 0 - none,	1 - 360 KB,	2 - 1.2 MB
; 3 - 720 KB,	4 - 1.44 MB,	6 - 2.88 MB
; Use two 1.44 MB drives by default, changed to 1 A-MSB B-LSB
%define DEFAULT_FLOPPIES 40h
