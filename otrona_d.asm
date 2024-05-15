; z80dasm 1.1.6
; command line: z80dasm -l -t -g 0 -a Otrona_Attaché_U252_Rev_D.BIN

; Note: in Mame, use ctrl-pagedown to get to monitor

	org	00000h

; To avoid using stack a lot of code uses ix as the return address
CALLIX: macro adrs
		ld ix,$+7	; 4 bytes for ld ix, 3 bytes for jp
		jp adrs
	endm
RCALLIX: macro adrs
		ld ix,$+6	; 4 bytes for ld ix, 2 bytes for jr
		jr adrs
	endm
CALLIY: macro adrs
		ld iy,$+7	; 4 bytes for ld iy, 3 bytes for jp
		jp adrs
	endm
RCALLIY: macro adrs
		ld iy,$+6	; 4 bytes for ld iy, 2 bytes for jr
		jr adrs
	endm


;
;|----------------------|
;|	CONSTANTS	|
;|----------------------|
;
;
;	SELECT DISK CONFIGURATION
;
DBL:	equ	1	;DENSITY (0=SINGLE, 1=DOUBLE)
FILSIZ:	equ	(DBL+1)*256
SECS:	equ	1	;SECTORS (0=9, 1=10)
SIDES:	equ	1	;SIDES   (0=SINGLE, 1=DOUBLE)
FILBUF:	equ	0FE00H	; LOCATION OF DISK BUFFER





; I/O ports, see technical manual page 3-28 (table 3-13)
FPYBCA: equ 0e0h	; FLOPPY STATUS PORT
FPYBWR:	equ 0e1h	; FLOPPY DATA PORT
DSPBCA:	equ 0e2h	; DISPLAY BASE & CURRENT ADDRESS
DSPBWR:	equ 0e3h	; DISPLAY BASE & WORD COUNT
STDBCA:	equ 0e4h	; STD BUS BASE & CURRENT ADDRESS
STDBWR:	equ 0e5h	; STD BUS BASE & WORD COUNT
SIOBCA:	equ 0e6h	; SIO BASE & CURRENT ADDRESS
SIOBWR:	equ 0e7h	; SIO BASE & WORD COUNT
DMACSR:	equ 0e8h	; DMA COMMAND/STATUS REGISTER
DMAWRR:	equ 0e9h	; DMA WRITE REQUEST REGISTER
DMAWSM:	equ 0eah	; DMA WRITE SINGLE MASK BIT
DMAWMR:	equ 0ebh	; DMA WRITE MODE REGISTER
DMACBP:	equ 0ech	; DMA CLEAR BYTE FLIP-FLOP
DMATMP:	equ 0edh	; DMA TEMP REG & MASTER CLEAR
SDSPY:	equ 0eeh	; DISPLAY COMMAND/STATUS
DMAWAM:	equ 0efh	; DMAWRITE ALL MASK REG BITS
DCOMM:	equ 0f0h	; COMM PORT DATA
SCOMM:	equ 0f1h	; COMM PORT STATUS
DPRTR:	equ 0f2h	; PRINTER PORT DATA
SPRTR:	equ 0f3h	; PRINTER PORT STATUS
BAUDC:	equ 0f4h	; BAUD TIMER FOR COMM PORT
BAUDP:	equ 0f5h	; BAUD TIMER FOR PRINTER PORT
DSPINT:	equ 0f6h	; DISPLAY INTERRUPr (60HZ)
FPYINT:	equ 0f7h	; FLOPPY INTERRUPT TIMER

DPIOA:	equ 0f8h	; PIO PORT A DATA
			; AO-7  = LATCH DATA OUT:
			; LO= MOTOR ON
			; L1= GRAPHICS ENABLE
			; L2= /EPROM ENABLE
			; L3-7  = DISPLAY BRIGHTNESS
			; AO-7  = 8910 DATA I/O:
			; AO-3  = 5832 DO-3 I/O
			; A4-7  = 5832 AO-3 OUT
			; AO-3  = 5101 DO-3 I/O
			; A4-7  = 5101 AO-3 OUT
SPIOA:	equ 0f9h	; PIO PORT A COMMAND
DPIOB:	equ 0fah	; PIO PORT B DATA
			; BO-1 = 5101 A4-5
			; B2-4 = OPERATION SELECT
			; 0= 8910 ADDR LOAD
			; 1= 8910 DATA LOAD
			; 2= 5832 WRITE
			; 3= 5832 READ
			; 4= 5101 WRITE
			; 5 = 5101 READ
			; 6 = LATCH LOAD
			; 7 = NO-OP
			; B5 = /1138 OPERATION STROBE
			; B6 = /KEYBOARD DATA IN
			; B7 = /KEYBOARD CLOCK OUT
SPIOB:	equ 0fbh	;PIO PORT B COMMAND
SFLPY:	equ 0fch	;FLOPPY COMMAND/STATUS
			; Bit 0: FDD 0 Busy
			; Bit 1: FDD 1 Busy
			; Bit 2: FDD 2 Busy
			; Bit 3: FDD 3 Busy
			; Bit 4: FDC Busy
			; Bit 5: Non-DMA mode
			; Bit 6: Data Input/Output
			; Bit 7: Request for Master

DFLPY:	equ 0fdh	;FLOPPY DATA
DDSPY:	equ 0feh	;DISPLAY DATA
DMAP:	equ 0ffh	;RAM VIRTUAL MAP DATA

; FLOPPY PARAMETERS

HLT:	equ 8		;HEAD LOAD TIME
HUT:	equ 0x0F	;HEAD UNLOAD TIME
ND:	equ 0		;DMA MODE
SRT:	equ 10		;STEP RATE = 12 MS
MTRTIM:	equ 300		;5S MOTOR TIMEOUT FOR TESTING ONLY
MTRDLY:	equ 45		;750MS. MOTOR START DELAY

; Additional information can be found in mame:
; https://github.com/mamedev/mame/blob/39b639d058275990289db08a159752d9c4f5d11f/src/mame/skeleton/attache.cpp

;   The stack
STACK_BASE: equ $fe00


; Ctrl-Lf
CTRLLF: equ 0x86

; ASCII Constants

BS: equ 0x08
LF: equ 0x0A
CR: equ 0x0D

RESET:
	jp INIT			; Init machine
	jp GOMON		; Go to monitor
	jp DISKOP
	jp DISPLY
	jp ERROR

SIGNON:
	db $1b, 'J'    	; ESC-J , probably erase to end of screen
    db "OTRONA ATTACHE\r", "\n"|0x80	; LF (with bit 7 for end of string)

	;   BOOT, we init the hardware
INIT:
	di
	ld hl,IOINITDATA	; I/O registers configuration table
LOOP_IOREG:
	ld a,(hl)			; Load I/O address to output
	ld c,a
	inc a
	jr z,REGTEST		; If address is 0xff, we're done
	inc hl
	ld b,(hl)			; Read number of bytes to output
	inc hl				;
LOOP_IODATA:
	ld a,(hl)			; Byte to output to I/O
	out (c),a			; Output to I/O register
	inc hl
	djnz LOOP_IODATA	; Still bytes for this register?
	jr LOOP_IOREG		; Go to next register

	;   Now we test if the CPU works well (this sounds quite strange)

	;   Test the registers by passing all 1s and all0s across all of them
REGTEST:	ld a,%11111111
	scf				; carry set
	ld i,a			; pass value throught i
	ld a,i
PASSVALUE:		  ; passes a value (all 1s, then all 0s, through all the registers)
	ld b,a				; pass value through b,c,d,e,l,h
	ld c,b
	ld d,c
	ld e,d
	ld l,e
	ld h,l
	ex af,af'			; value goes to other register bank
	ld a,h				; a contains the passed value
	exx		 ; now same exercice with the other bank
	ld b,a
	ld c,b
	ld d,c
	ld e,d
	ld l,e
	ld h,l
	ex af,af'			; And get the other a to fill, and the carry
	ld a,h				; and fill it
	jr nc,OTHER_TEST		; second loop? (c was 1 first time)
	cpl				; swap 0 and 1
	and a				; test
	jr z,PASSVALUE		; should be zero, and a is now %00000000
			; carry is not set, so we'll go to OTHER_TEST if everything is good

CPUERR:
	halt				; We failed to boot

OTHER_TEST:
	jr nz,CPUERR	; Second time, a should be zero
	or a				; Test that "or 0" is 0.
	jr nz,CPUERR

	;   Test the ix, iy, sp registers and dual byte additions/flags
			; note: all registers are 0
	ld ix,$ffff			;
	ld sp,ix			;
	add hl,sp			; hl was 0, so should now be $ffff
	ex de,hl			; de=$ffff, hl=0

	ld iy,$ffff			; same dance with iy
	ld sp,iy			;
	add hl,sp			; $ffff
	add hl,de			; $fffe
	jr nc,CPUERR	; We clearly had a carry, right?

	inc hl				; now $ffff
	ld ix,0
	ld sp,ix
	add hl,sp	   ; no change
	ex de,hl			; de=$ffff, hl=$ffff

	ld iy,0
	ld sp,iy
	add hl,sp			; $ffff
	add hl,de			; $fffe
	inc hl				; $ffff
	ld a,h				; $ff
	xor l				; $ff^$ff = $00
	jr nz,CPUERR	; if we did not land on zero, something wrong

	;   Test basic flags behavior
	ld a,$40
	add a,a
	jr z,CPUERR	; $80 is not 0
	jr c,CPUERR	; $40+$40 does not create carry
	jp p,CPUERR	; $80 is -128, so if positive, failed
	jp po,CPUERR	; not overflow is tested (parity and overflow shares a flag)
	add a,a				; $80+$80 = $00
	jr nz,CPUERR	; Not zero? Fail
	jr nc,CPUERR	; If no carry, fail
	jp m,CPUERR	; If negative, fail
	or 1			; aka or a
	jp pe,CPUERR	; parity is even? Fail

	;   Test decimal adjustment instruction
	ld a,9
	add a,1
	daa		 ; decimal 10 adjusted to decimal
	cp 010h			; is hex 10
	jr nz,CPUERR	; or Fail
	sbc a,001h			; Carry was set to zero by daa, a is $f
	daa				; hex f adjusted is 9
	cp 9
	jr nz,CPUERR	; not 9? Fail

	;   (unsure)
	ld sp,STACK_BASE	; Initial stack
	ld hl,MEMMAP8TOF
	CALLIX MAPMEM

	xor a
	ld i,a

		;	Start Block test (FD00-FDFF)
	ld hl,0fd00h
_L1:
	ld c,0ffh			; Store ff, 7f, 3f, ... 03, 01 in memory
_L2:
	ld (hl),c			; Store c
	ld a,c
	xor (hl)			; Invert
	jp nz,MEMERR		; Should be 00
	sla c				;
	jr c,_L2			;
	inc l				; next byte
	jr nz,_L1

			;	Set initial values
	cpl
	ld (SHLOCK),a		; No shift-lock
	ld a,0c0h
	ld (LSTATE),a		; Initial LSTATE

		;	"Reinitialize display if 50Hz"
	ld d,2
	call RDCMOS		; Read adress 2 (50Hz flag)
	ld a,c
	cp 9
	jr nz,_J0		;	No 50Hz?
	ld a,0xa4		; (unclear)
	out (SDSPY),a	;
	ld a,0x01d		;
	out (DDSPY),a	;
	ld a,0xa5		;
	out (SDSPY),a	;
	ld a,0x25		;
	out (DDSPY),a	;
_J0:
	ld a,00fh		; setup interrupt vector to 0x0f
	ld i,a
	im 2		; interrupt mode 2,
			    ; CPU will jump at 0xfnn,
				; with nn selected by the device during the Interrupt Response Cycle
	ei			; enable interrupts
	ld hl,SIGNON
	CALLIY DISP_HL
	jp BOOT



MNTR:
	ld c,"\r"
	CALLIX COIX
	ld c,"\n"
	CALLIX COIX
	ld c,'@'
	CALLIX COIX
	ld de,0
	ld (SAVDE),de

MNTR2:
	ld h,0
	ld l,h
	ld b,h
l012ah:
	ld a,i		;012a	ed 57 	. W
	or a			;012c	b7 	.
	jr z,l0173h		;012d	28 44 	( D

	call ABTTST			; Test for abort (?)
	ld a,(FLGCMD)		; Test for macro
	or a
	jr z,l0148h			; No macro in progress
	push hl				; Next command
	ld hl,(CMDPTR)
	ld a,(hl)
	inc hl
	ld (CMDPTR),hl
	pop hl
	or a				;
	jr nz,l017eh		; Execute command
	ld (FLGCMD),a		; End of macro (a=0)
l0148h:
	ld a,(FLGLOP)		;0148	3a 80 fd 	: . .
	cp 080h		;014b	fe 80 	. .
	jr nz,l0161h		;014d	20 12 	  .
	ld hl,CMDSTT		;014f	21 83 fd 	! . .
	ld (CMDPTR),hl		;0152	22 7e fd 	" ~ .
	ld hl,(SAVHL2)		;0155	2a 8d fd 	* . .
	ld de,(SAVDE2)		;0158	ed 5b 8b fd 	. [ . .
	ld a,(SVCMD2)		;015c	3a 8f fd 	: . .
	jr l017eh		;015f	18 1d 	. .
l0161h:
	CALLIY l081ah
	jr z,l0173h		;0168	28 09 	( .
	xor a			;016a	af 	.
	ld (KEYFLG),a		;016b	32 79 fd 	2 y .
	ld a,(KEYCOD)		;016e	3a 78 fd 	: x .
	jr l017eh		;0171	18 0b 	. .
l0173h:
	CALLIY l0790h
	jr z,l0161h		;017a	28 e5 	( .
	in a,(DCOMM)		;017c	db f0 	. .
l017eh:
	bit 6,a				; Uppercase
	jr z,ECHO
	res 5,a		;0182	cb af 	. .
ECHO:
	ld c,a			;0184	4f 	O
	CALLIX COIX
	ld (SAVCMD),a		;018c	32 8a fd 	2 . .

;
; Monitor commands
;
CBS:					; BS = Examine previous memory address
	cp BS
	jr nz,CCR
	ex de,hl
	xor a
	or b				; b == data present & e = new data ? ####checkme!
	jr z,_J1
	ld (hl),e
_J1:
	dec hl				; Previous address
	jr LFRET			; Ends like "LF"

CCR:					; CR = Close and back to monitor
	cp CR
	jr nz,CESC
	xor a
	or b				; Check if update needed
	jp z,GOMON
	ex de,hl
	ld (hl),e			; Update and back
	jp GOMON

CESC:				; CTRL LINE FEED = Back to terminal
	cp CTRLLF
	jp z,GOTERM

CLF:				; LF = Examine next memory address
	cp LF
	jr nz,CSLASH
	ex de,HL
	xor a
	or b
	jr z,_J1
	ld (hl),e		; Update memory if needed
_J1:
	inc hl			; Next address

LFRET:
	ld c,CR
	CALLIX COIX
	CALLIY DSH		; Display h
	ld a,h
	ld h,l
	ld l,a
	CALLIY DSH		; Display l
	ld a,l
	ld l,h
	ld h,a
	ld a,'/'
	jr ECHO

CSLASH:				; / = Open and display
	cp '/'
	jr nz,CSPC
	ld b,0
	ld d,h
	ld e,l
	ld h,(hl)		; Display memort
	CALLIY DSH
	ld c,' '
	CALLIX COIX
	ld h,0
	ld l,h
	jp l012ah

CSPC:				; Unsure what it does, to be honest
	cp ' '
	jr nz,CGRAPH
	ld (SAVDE),hl		;	Unsure why
	ex de,hl
	jp MNTR2

CGRAPH:
	cp 'G'          ; Generate Display Pattern
	jr nz,CALPHA

;
;	G - PRESENT DISPLAY TEST PATTERN
;	& PROVIDE CLOCK SET-UP OUTPUT
;
	ld c,0x1e		; HOME
	call DISPLY
	ld de,2175
	ld c,'+'
		; loop 2175 times
_L1:
	call DISPLY
	dec e
	jr nz,_L1
	dec d
	jr nz,_L1


	call sub_065ah		;021c	cd 5a 06 	. Z .
	bit 7,d		;021f	cb 7a 	. z
	jr z,l0236h		;0221	28 13 	( .
	ld d,0cfh		;0223	16 cf 	. .
	call RDCMOS		;0225	cd cb 06 	. . .
	ld a,088h		;0228	3e 88 	> .
	out (DPIOB),a		;022a	d3 fa 	. .
l022ch:
	in a,(DPIOB)		;022c	db fa 	. .
	bit 6,a		;022e	cb 77 	. w
	jr z,l022ch		;0230	28 fa 	( .
	ei			;0232	fb 	.
	jp ABORT		;0233	c3 20 06 	.   .
l0236h:
	ld d,04fh		;0236	16 4f 	. O
	call sub_06bbh		;0238	cd bb 06 	. . .
	ei			;023b	fb 	.
	call ABTTST		;023c	cd 1c 06 	. . .
	jr l0236h		;023f	18 f5 	. .

CALPHA:
	cp 'H'          ; Display RAM test
	jr nz,l028dh		;0243	20 48 	  H
	ld e,000h		;0245	1e 00 	. .
l0247h:
	ld c,0feh		;0247	0e fe 	. .
	ld d,080h		;0249	16 80 	. .
	ld a,e			;024b	7b 	{
	out (SDSPY),a		;024c	d3 ee 	. .
	ld b,000h		;024e	06 00 	. .
l0250h:
	in h,(c)		;0250	ed 60 	. `
	out (c),d		;0252	ed 51 	. Q
	in a,(c)		;0254	ed 78 	. x
	out (c),h		;0256	ed 61 	. a
	cp d			;0258	ba 	.
	jr z,l026bh		;0259	28 10 	( .
	xor d			;025b	aa 	.
	ld d,a			;025c	57 	W
	ld a,e			;025d	7b 	{
	and 01fh		;025e	e6 1f 	. .
	ld h,a			;0260	67 	g
	ld a,e			;0261	7b 	{
	and 0e0h		;0262	e6 e0 	. .
	ld e,a			;0264	5f 	_
	ld l,b			;0265	68 	h
	call sub_0685h		;0266	cd 85 06 	. . .
	jr l027dh		;0269	18 12 	. .
l026bh:
	rrc d		;026b	cb 0a 	. .
	jr nc,l0250h		;026d	30 e1 	0 .
	inc b			;026f	04 	.
	ld a,b			;0270	78 	x
	cp 050h		;0271	fe 50 	. P
	jr nz,l0250h		;0273	20 db 	  .
	inc e			;0275	1c 	.
	ld a,e			;0276	7b 	{
	and 01fh		;0277	e6 1f 	. .
	cp 018h		;0279	fe 18 	. .
	jr nz,l0247h		;027b	20 ca 	  .
l027dh:
	ld a,e			;027d	7b 	{
	and 0e0h		;027e	e6 e0 	. .
l0280h:
	add a,020h		;0280	c6 20 	.
	cp 0a0h		;0282	fe a0 	. .
	jr z,l0280h		;0284	28 fa 	( .
	or a			;0286	b7 	.
	ld e,a			;0287	5f 	_
	jr nz,l0247h		;0288	20 bd 	  .
	jp l05f8h		;028a	c3 f8 05 	. . .
l028dh:
	cp 'I'          ; Input Test
	jr nz,l029ah		;028f	20 09 	  .
	ld c,l			;0291	4d 	M
	in h,(c)		;0292	ed 60 	. `
	call sub_0696h		;0294	cd 96 06 	. . .
	jp l05f8h		;0297	c3 f8 05 	. . .
l029ah:
	cp 'J'          ; Jump
	jr nz,l02a3h		;029c	20 05 	  .
	ld de,MNTR		;029e	11 04 01 	. . .
	push de			;02a1	d5 	.
	jp (hl)			;02a2	e9 	.
l02a3h:
	cp 'K'          ; Keyboard Test
	jr nz,l02bch		;02a5	20 15 	  .
l02a7h:
	call sub_076dh		;02a7	cd 6d 07 	. m .
	cp 05eh		;02aa	fe 5e 	. ^
	jp z,GOMON		;02ac	ca 2f 06 	. / .
	ld c,a			;02af	4f 	O
	call DISPLY
	ld h,a			;02b3	67 	g
	call sub_0696h		;02b4	cd 96 06 	. . .
	call SPACE
	jr l02a7h		;02ba	18 eb 	. .
l02bch:
	cp 'L'			; Loop tests
	jr nz,l02c8h
	ld a,0xff
	ld (FLGLOP),a		;02c2	32 80 fd 	2 . .
	jp GOMON		;02c5	c3 2f 06 	. / .
l02c8h:
	cp 'M'			;	Memory Map Test
	jr nz,l031ah		;02ca	20 4e 	  N
	; Map Test
	di
	ld hl,MEMMAPFTO8
	CALLIX MAPMEM
	ld hl,0		;02d7	21 00 00 	! . .
	ld b,007h		;02da	06 07 	. .
l02dch:
	ld (hl),b			;02dc	70 	p
	ld a,h			;02dd	7c 	|
	add a,020h		;02de	c6 20 	.
	ld h,a			;02e0	67 	g
	djnz l02dch		;02e1	10 f9 	. .
	ld hl,MEMMAP8TOF		;02e3	21 53 0f 	! S .
	CALLIX MAPMEM
	ld a,i		;02ed	ed 57 	. W
	or a			;02ef	b7 	.
	jr z,l02f3h		;02f0	28 01 	( .
	ei			;02f2	fb 	.
l02f3h:
	ld hl,02000h		;02f3	21 00 20 	! .
	ld b,001h		;02f6	06 01 	. .
l02f8h:
	ld a,(hl)			;02f8	7e 	~
	cp b			;02f9	b8 	.
	jr z,l0310h		;02fa	28 14 	( .
	ld d,a			;02fc	57 	W
	ld a,h			;02fd	7c 	|
	or 030h		;02fe	f6 30 	. 0
	ld c,a			;0300	4f 	O
	CALLIX COIX
	ld h,d			;0308	62 	b
	CALLIY DSH		;030d	c3 98 06 	. . .
l0310h:
	inc b			;0310	04 	.
	ld a,h			;0311	7c 	|
	add a,020h		;0312	c6 20 	.
	ld h,a			;0314	67 	g
	jr nz,l02f8h		;0315	20 e1 	  .
	jp l05f8h		;0317	c3 f8 05 	. . .
l031ah:
	cp 'N'			; Mystery item. Warm boot?
	jr nz,l0321h		;031c	20 03 	  .
	jp GOMON		;031e	c3 2f 06 	. / .
l0321h:
	cp 'O'			; Output Test
	jr nz,l032bh		;0323	20 06 	  .
	ld c,h			;0325	4c 	L
	out (c),l		;0326	ed 69 	. i
	jp l05f8h		;0328	c3 f8 05 	. . .
l032bh:
	cp 'P'			; Format Diskette
	jr nz,l0335h		;032d	20 06 	  .
	call SELFMT		;032f	cd b5 09 	. . .
	jp GOMON		;0332	c3 2f 06 	. / .
l0335h:
	cp 'Q'			; CMOS Memory Test
	jp nz,l036eh		;0337	c2 6e 03 	. n .
	ld e,00fh		;033a	1e 0f 	. .
l033ch:
	call RDCMOS		;033c	cd cb 06 	. . .
	ld l,c			;033f	69 	i
	ld b,000h		;0340	06 00 	. .
l0342h:
	ld c,b			;0342	48 	H
	call sub_06cfh		;0343	cd cf 06 	. . .
	call RDCMOS		;0346	cd cb 06 	. . .
	ld h,c			;0349	61 	a
	ld c,l			;034a	4d 	M
	call sub_06cfh		;034b	cd cf 06 	. . .
	ei			;034e	fb 	.
	ld a,h			;034f	7c 	|
	and 00fh		;0350	e6 0f 	. .
	xor b			;0352	a8 	.
	jr nz,l0363h		;0353	20 0e 	  .
	inc b			;0355	04 	.
	ld a,b			;0356	78 	x
	cp 010h		;0357	fe 10 	. .
	jr nz,l0342h		;0359	20 e7 	  .
	inc d			;035b	14 	.
	ld a,d			;035c	7a 	z
	cp 040h		;035d	fe 40 	. @
	jr nz,l033ch		;035f	20 db 	  .
	jr l036bh		;0361	18 08 	. .
l0363h:
	ld h,d			;0363	62 	b
	ld l,a			;0364	6f 	o
	call SPACE
	call sub_0692h		;0368	cd 92 06 	. . .
l036bh:
	jp l05f8h		;036b	c3 f8 05 	. . .
l036eh:
	cp 'R'			; Main.Memory Test
	jp nz,l043bh		;0370	c2 3b 04 	. ; .
	di			;0373	f3 	.
	ld a,l			;0374	7d 	}
	and 003h		;0375	e6 03 	. .
	ld a,l			;0377	7d 	}
	jr nz,l0385h		;0378	20 0b 	  .
	ld hl,0fd00h		;037a	21 00 fd 	! . .
	ld de,09d00h		;037d	11 00 9d 	. . .
	ld bc,00100h		;0380	01 00 01 	. . .
	ldir		;0383	ed b0 	. .
l0385h:
	ld hl,l0f3fh		;0385	21 3f 0f 	! ? .
	ld d,a			;0388	57 	W
	and 003h		;0389	e6 03 	. .
	rlca			;038b	07 	.
	rlca			;038c	07 	.
	ld c,a			;038d	4f 	O
	ld b,000h		;038e	06 00 	. .
	add hl,bc			;0390	09 	.
	CALLIX MAPMEM
	ld a,i		;0398	ed 57 	. W
	or a			;039a	b7 	.
	jr z,l03a0h		;039b	28 03 	( .
	ei			;039d	fb 	.
	ld b,009h		;039e	06 09 	. .
l03a0h:
	ld e,000h		;03a0	1e 00 	. .
l03a2h:
	ld hl,08000h		;03a2	21 00 80 	! . .
l03a5h:
	ld a,l			;03a5	7d 	}
	xor h			;03a6	ac 	.
	xor e			;03a7	ab 	.
	ld (hl),a			;03a8	77 	w
	inc hl			;03a9	23 	#
	ld a,h			;03aa	7c 	|
	cp 0c0h		;03ab	fe c0 	. .
	jr nz,l03a5h		;03ad	20 f6 	  .
	di			;03af	f3 	.
	ld h,0e0h		;03b0	26 e0 	& .
l03b2h:
	inc hl			;03b2	23 	#
	ld a,h			;03b3	7c 	|
	or l			;03b4	b5 	.
	jr nz,l03b2h		;03b5	20 fb 	  .
	ld a,i		;03b7	ed 57 	. W
	or a			;03b9	b7 	.
	jr z,l03bdh		;03ba	28 01 	( .
	ei			;03bc	fb 	.
l03bdh:
	ld h,080h		;03bd	26 80 	& .
l03bfh:
	ld a,l			;03bf	7d 	}
	xor h			;03c0	ac 	.
	xor e			;03c1	ab 	.
	xor (hl)			;03c2	ae 	.
	jr nz,MEMERR		;03c3	20 14 	  .
l03c5h:
	inc hl			;03c5	23 	#
	ld a,h			;03c6	7c 	|
	cp 0c0h		;03c7	fe c0 	. .
	jr nz,l03bfh		;03c9	20 f4 	  .
	ld ix,l03d2h		;03cb	dd 21 d2 03 	. ! . .
	jp l079ch		;03cf	c3 9c 07 	. . .
l03d2h:
	jr nz,l0417h		;03d2	20 43 	  C
	inc e			;03d4	1c 	.
	jr nz,l03a2h		;03d5	20 cb 	  .
	jr l0417h		;03d7	18 3e 	. >
MEMERR:
	ex af,af'			;03d9	08 	.
	ld c,020h		;03da	0e 20 	.
	CALLIX COIX
	ld iy,003eah		;03e3	fd 21 ea 03 	. ! . .
	res 7,h		;03e7	cb bc 	. .
	jp DSH		;03e9	c3 98 06 	. . .
	set 7,h		;03ec	cb fc 	. .
	ld a,h			;03ee	7c 	|
	ld h,l			;03ef	65 	e
	ld l,a			;03f0	6f 	o
	CALLIY DSH		;03f5	c3 98 06 	. . .
	ld c,02dh		;03f8	0e 2d 	. -
	CALLIX COIX
	ld a,h			;0401	7c 	|
	ex af,af'			;0402	08 	.
	ld h,a			;0403	67 	g
	CALLIY DSH		;0408	c3 98 06 	. . .
	ld h,l			;040b	65 	e
	ex af,af'			;040c	08 	.
	ld l,a			;040d	6f 	o
	bit 4,d		;040e	cb 62 	. b
	jr z,l0415h		;0410	28 03 	( .
l0412h:
	ld (hl),a			;0412	77 	w
	jr l0412h		;0413	18 fd 	. .
l0415h:
	djnz l03c5h		;0415	10 ae 	. .
l0417h:
	di			;0417	f3 	.
	ld hl,MEMMAP8TOF		;0418	21 53 0f 	! S .
	CALLIX MAPMEM
	ld a,d			;0422	7a 	z
	and 003h		;0423	e6 03 	. .
	jr nz,l0432h		;0425	20 0b 	  .
	ld hl,09d00h		;0427	21 00 9d 	! . .
	ld de,TYPE		;042a	11 00 fd 	. . .
	ld bc,00100h		;042d	01 00 01 	. . .
	ldir		;0430	ed b0 	. .
l0432h:
	ld a,i		;0432	ed 57 	. W
	or a			;0434	b7 	.
	jr z,l0438h		;0435	28 01 	( .
	ei			;0437	fb 	.
l0438h:
	jp l05f8h		;0438	c3 f8 05 	. . .
l043bh:
	cp 'S'			; Select Output Port
	jr nz,l0469h		;043d	20 2a 	  *
	ld a,l			;043f	7d 	}
	ld (OUTSEL),a		;0440	32 85 fd 	2 . .
	ld a,h			;0443	7c 	|
	ld c,0f4h		;0444	0e f4 	. .
	call sub_0456h		;0446	cd 56 04 	. V .
	ld a,h			;0449	7c 	|
	rrca			;044a	0f 	.
	rrca			;044b	0f 	.
	rrca			;044c	0f 	.
	rrca			;044d	0f 	.
	ld c,0f5h		;044e	0e f5 	. .
	call sub_0456h		;0450	cd 56 04 	. V .
	jp GOMON		;0453	c3 2f 06 	. / .
sub_0456h:
	and 00fh		;0456	e6 0f 	. .
	ret z			;0458	c8 	.
	push hl			;0459	e5 	.
	ld hl,00f2dh		;045a	21 2d 0f 	! - .
	ld d,000h		;045d	16 00 	. .
	ld e,a			;045f	5f 	_
	add hl,de			;0460	19 	.
	ld b,057h		;0461	06 57 	. W
	out (c),b		;0463	ed 41 	. A
	outi		;0465	ed a3 	. .
	pop hl			;0467	e1 	.
	ret			;0468	c9 	.
l0469h:
	cp 'T'			; Real-Time Clock Test
	jr nz,l0489h		;046b	20 1c 	  .
	call sub_065ah		;046d	cd 5a 06 	. Z .
	call sub_06bbh		;0470	cd bb 06 	. . .
	ld h,c			;0473	61 	a
	ld b,07dh		;0474	06 7d 	. }
	call DELAY
	call sub_06bbh		;0479	cd bb 06 	. . .
	ei			;047c	fb 	.
	ld a,c			;047d	79 	y
	cp 00fh		;047e	fe 0f 	. .
	jr z,l0483h		;0480	28 01 	( .
	cp h			;0482	bc 	.
l0483h:
	jp z,l0615h		;0483	ca 15 06 	. . .
	jp l05f8h		;0486	c3 f8 05 	. . .
l0489h:
	cp 'U'			; United Tests
	jr nz,l04cbh		;048b	20 3e 	  >
	ld a,(FLGLOP)		;048d	3a 80 fd 	: . .
	cp 080h		;0490	fe 80 	. .
	jr nz,l0499h		;0492	20 05 	  .
	ld hl,(CMDSTT)		;0494	2a 83 fd 	* . .
	jr l04c0h		;0497	18 27 	. '
l0499h:
	ld hl,CMDBUF		;0499	21 90 fd 	! . .
	push hl			;049c	e5 	.
	ld (CMDPTR),hl		;049d	22 7e fd 	" ~ .
l04a0h:
	ld (hl),000h		;04a0	36 00 	6 .
	call sub_076dh		;04a2	cd 6d 07 	. m .
	ld c,a			;04a5	4f 	O
	call CO		;04a6	cd e1 07 	. . .
	cp 00dh		;04a9	fe 0d 	. .
	jr z,l04b1h		;04ab	28 04 	( .
	ld (hl),a			;04ad	77 	w
	inc hl			;04ae	23 	#
	jr l04a0h		;04af	18 ef 	. .
l04b1h:
	pop de			;04b1	d1 	.
	and a			;04b2	a7 	.
	sbc hl,de		;04b3	ed 52 	. R
	ld a,h			;04b5	7c 	|
	or l			;04b6	b5 	.
	jr nz,l04bch		;04b7	20 03 	  .
	ld de,UNITSEQ		;04b9	11 57 0f 	. W .
l04bch:
	ex de,hl			;04bc	eb 	.
	ld (CMDSTT),hl		;04bd	22 83 fd 	" . .
l04c0h:
	ld (CMDPTR),hl		;04c0	22 7e fd 	" ~ .
	ld a,0ffh		;04c3	3e ff 	> .
	ld (FLGCMD),a		;04c5	32 81 fd 	2 . .
	jp l05f8h		;04c8	c3 f8 05 	. . .
l04cbh:
	cp 'V'				; Read Disk Sector
	jr nz,l04e1h		;04cd	20 12 	  .
	ld ix,ERROR
	call FPREAD
l04d6h:
	ld a,(ST0)		;04d6	3a 15 fd 	: . .
	and 0c0h		;04d9	e6 c0 	. .
	call nz,sub_0913h		;04db	c4 13 09 	. . .
	jp l05f8h		;04de	c3 f8 05 	. . .
l04e1h:
	cp 'W'				; write Disk Sector
	jr nz,l04eeh		;04e3	20 09 	  .
	ld ix,ERROR
	call FPWRIT
	jr l04d6h		;04ec	18 e8 	. .
l04eeh:
	cp 'X'				; I/O Port Transmit Test
	jr nz,l051bh		;04f0	20 29 	  )
	ld c,00dh		;04f2	0e 0d 	. .
	call sub_07dbh		;04f4	cd db 07 	. . .
l04f7h:
	ld a,(hl)			;04f7	7e 	~
	rrca			;04f8	0f 	.
	rrca			;04f9	0f 	.
	rrca			;04fa	0f 	.
	rrca			;04fb	0f 	.
	and 00fh		;04fc	e6 0f 	. .
	or 030h		;04fe	f6 30 	. 0
	ld c,a			;0500	4f 	O
	call sub_07dbh		;0501	cd db 07 	. . .
	ld a,(hl)			;0504	7e 	~
	and 00fh		;0505	e6 0f 	. .
	or 030h		;0507	f6 30 	. 0
	ld c,a			;0509	4f 	O
	call sub_07dbh		;050a	cd db 07 	. . .
	dec de			;050d	1b 	.
	inc hl			;050e	23 	#
	ld a,d			;050f	7a 	z
	or e			;0510	b3 	.
	jr nz,l04f7h		;0511	20 e4 	  .
	ld c,00dh		;0513	0e 0d 	. .
	call sub_07dbh		;0515	cd db 07 	. . .
	jp l05f8h		;0518	c3 f8 05 	. . .
l051bh:
	cp 'Y'			; I/O Port Receive Test
	jr nz,l053fh		;051d	20 20
l051fh:
	call CIPI		;051f	cd ac 07 	. . .
	cp 00dh		;0522	fe 0d 	. .
	jr nz,l051fh		;0524	20 f9 	  .
l0526h:
	call CIPI		;0526	cd ac 07 	. . .
	cp 00dh		;0529	fe 0d 	. .
	jp z,l05f8h		;052b	ca f8 05 	. . .
	and 00fh		;052e	e6 0f 	. .
	rlca			;0530	07 	.
	rlca			;0531	07 	.
	rlca			;0532	07 	.
	rlca			;0533	07 	.
	ld d,a			;0534	57 	W
	call CIPI		;0535	cd ac 07 	. . .
	and 00fh		;0538	e6 0f 	. .
	or d			;053a	b2 	.
	ld (hl),a			;053b	77 	w
	inc hl			;053c	23 	#
	jr l0526h		;053d	18 e7 	. .
l053fh:
	cp 'Z'			; Disk Drive Test
	jp nz,l0635h		;0541	c2 35 06 	. 5 .
	call SELFMT		;0544	cd b5 09 	. . .
	xor a			;0547	af 	.
	ld (ERRCNT),a		;0548	32 1d fd 	2 . .
	res 6,l		;054b	cb b5 	. .
l054dh:
	ld a,l			;054d	7d 	}
	and 0d0h		;054e	e6 d0 	. .
	inc a			;0550	3c 	<
	ld l,a			;0551	6f 	o
l0552h:
	push hl			;0552	e5 	.
	ld hl,STACK_BASE		;0553	21 00 fe 	! . .
	ld bc,00200h		;0556	01 00 02 	. . .
l0559h:
	ld (hl),0dbh		;0559	36 db 	6 .
	cpi		;055b	ed a1 	. .
	ld (hl),0e6h		;055d	36 e6 	6 .
	cpi		;055f	ed a1 	. .
	jp pe,l0559h		;0561	ea 59 05 	. Y .
	pop hl			;0564	e1 	.
	ld h,028h		;0565	26 28 	& (
	bit 6,l		;0567	cb 75 	. u
	jr z,l056dh		;0569	28 02 	( .
	ld h,024h		;056b	26 24 	& $
l056dh:
	bit 7,l		;056d	cb 7d 	. }
	jr z,l0579h		;056f	28 08 	( .
	ld h,050h		;0571	26 50 	& P
	bit 6,l		;0573	cb 75 	. u
	jr z,l0579h		;0575	28 02 	( .
	ld h,048h		;0577	26 48 	& H
l0579h:
	dec h			;0579	25 	%
	call ABTTST		;057a	cd 1c 06 	. . .
	ld ix,ERROR
	call FPWRIT
	ld a,(ST0)		;0584	3a 15 fd 	: . .
	and 0c0h		;0587	e6 c0 	. .
	call nz,sub_0913h		;0589	c4 13 09 	. . .
	ld a,h			;058c	7c 	|
	or a			;058d	b7 	.
	jr nz,l0579h		;058e	20 e9 	  .
l0590h:
	push hl			;0590	e5 	.
	call ABTTST		;0591	cd 1c 06 	. . .
	ld ix,ERROR
	call FPREAD
	ld a,(ST0)		;059b	3a 15 fd 	: . .
	and 0c0h		;059e	e6 c0 	. .
	call nz,sub_0913h		;05a0	c4 13 09 	. . .
	ld hl,STACK_BASE		;05a3	21 00 fe 	! . .
	ld bc,00200h		;05a6	01 00 02 	. . .
l05a9h:
	ld a,0dbh		;05a9	3e db 	> .
	cpi		;05ab	ed a1 	. .
	jr nz,l05bbh		;05ad	20 0c 	  .
	ld a,0e6h		;05af	3e e6 	> .
	cpi		;05b1	ed a1 	. .
	jr nz,l05bbh		;05b3	20 06 	  .
	jp pe,l05a9h		;05b5	ea a9 05 	. . .
	pop hl			;05b8	e1 	.
	jr l05cah		;05b9	18 0f 	. .
l05bbh:
	pop hl			;05bb	e1 	.
	ld a,(ST0)		;05bc	3a 15 fd 	: . .
	and 0c0h		;05bf	e6 c0 	. .
	ld (ST0),a		;05c1	32 15 fd 	2 . .
	ld (ST1),a		;05c4	32 16 fd 	2 . .
	call z,sub_0913h		;05c7	cc 13 09 	. . .
l05cah:
	inc h			;05ca	24 	$
	ld a,h			;05cb	7c 	|
	cp 050h		;05cc	fe 50 	. P
	jr z,l05e8h		;05ce	28 18 	( .
	cp 048h		;05d0	fe 48 	. H
	jr nz,l05d8h		;05d2	20 04 	  .
	bit 6,l		;05d4	cb 75 	. u
	jr z,l0590h		;05d6	28 b8 	( .
l05d8h:
	bit 7,l		;05d8	cb 7d 	. }
	jr nz,l0590h		;05da	20 b4 	  .
	cp 028h		;05dc	fe 28 	. (
	jr z,l05e8h		;05de	28 08 	( .
	cp 024h		;05e0	fe 24 	. $
	jr nz,l0590h		;05e2	20 ac 	  .
	bit 6,l		;05e4	cb 75 	. u
	jr z,l0590h		;05e6	28 a8 	( .
l05e8h:
	inc l			;05e8	2c 	,
	ld a,l			;05e9	7d 	}
	and 00fh		;05ea	e6 0f 	. .
	cp 00bh		;05ec	fe 0b 	. .
	jp nz,l0552h		;05ee	c2 52 05 	. R .
	bit 6,l		;05f1	cb 75 	. u
	set 6,l		;05f3	cb f5 	. .
	jp z,l054dh		;05f5	ca 4d 05 	. M .
l05f8h:
	ld a,(FLGLOP)		;05f8	3a 80 fd 	: . .
	or a			;05fb	b7 	.
	jr z,l0612h		;05fc	28 14 	( .
	cp 080h		;05fe	fe 80 	. .
	jr z,l0612h		;0600	28 10 	( .
	ld a,080h		;0602	3e 80 	> .
	ld (FLGLOP),a		;0604	32 80 fd 	2 . .
	ld hl,SAVDE		;0607	21 86 fd 	! . .
	ld de,SAVDE2		;060a	11 8b fd 	. . .
	ld bc,00005h		;060d	01 05 00 	. . .
	ldir		;0610	ed b0 	. .
l0612h:
	jp GOMON		;0612	c3 2f 06 	. / .
l0615h:
	ld c,'?'
	call CO
	jr GOMON

;
;	TEST FOR ABORT FROM CONSOLE
;
ABTTST:
	call XXXKEYSTS
	ret z			;061f	c8 	.

ABORT:
	CALLIX l076fh
	ld a,0
	ld (FLGLOP),a		; No loop pending
	ld (FLGCMD),a		; No macro command

GOMON:
	ld sp,STACK_BASE
	jp MNTR

		; continuation of diagnostic input: unrecognized key
l0635h:
	sub 030h		;0635	d6 30 	. 0
	jr c,l0646h		;0637	38 0d 	8 .
	cp 00ah		;0639	fe 0a 	. .
	ccf			;063b	3f 	?
	jp m,l0646h		;063c	fa 46 06 	. F .
	cp 011h		;063f	fe 11 	. .
	sub 007h		;0641	d6 07 	. .
	cp 010h		;0643	fe 10 	. .
	ccf			;0645	3f 	?
l0646h:
	jp c,l012ah		;0646	da 2a 01 	. * .
	add hl,hl			;0649	29 	)
	add hl,hl			;064a	29 	)
	add hl,hl			;064b	29 	)
	add hl,hl			;064c	29 	)
	or l			;064d	b5 	.
	ld l,a			;064e	6f 	o
	inc b			;064f	04 	.
	ld (SAVHL),hl		;0650	22 88 fd 	" . .
	ld (SAVDE),de		;0653	ed 53 86 fd 	. S . .
	jp l012ah		;0657	c3 2a 01 	. * .
sub_065ah:
	ld c,009h		;065a	0e 09 	. .
	ld d,04fh		;065c	16 4f 	. O
	call sub_06cfh		;065e	cd cf 06 	. . .
	call sub_06bbh		;0661	cd bb 06 	. . .
	ld h,a			;0664	67 	g
	ld a,c			;0665	79 	y
	cp 00fh		;0666	fe 0f 	. .
	jr z,l0678h		;0668	28 0e 	( .
	ld d,040h		;066a	16 40 	. @
	ld c,007h		;066c	0e 07 	. .
	call sub_06cfh		;066e	cd cf 06 	. . .
	ld d,04eh		;0671	16 4e 	. N
	ld c,001h		;0673	0e 01 	. .
	call sub_06cfh		;0675	cd cf 06 	. . .
l0678h:
	ld d,h			;0678	54 	T
	ret			;0679	c9 	.0f2c

DELAY:
	push bc
_L1:
	push bc
_L2:
	djnz _L2
	pop bc
	djnz _L1
	pop bc
	djnz DELAY
	ret

sub_0685h:
	call SPACE
	call sub_0692h		;0688	cd 92 06 	. . .
	ld c,'-'
	call CO
	ld h,d			;0690	62 	b
	ld l,e			;0691	6b 	k
sub_0692h:
	call sub_0696h		;0692	cd 96 06 	. . .
	ld h,l			;0695	65 	e
sub_0696h:
	pop iy		;0696	fd e1 	. .


;	DISPLAY H IN HEX
;	(AC) [Y]
DSH:
	ld a,h
	rlca
	rlca
	rlca
	rlca
	RCALLIX DSH1		;06a1	18 09 	. .
	ld a,h
	RCALLIX DSH1		;06a8	18 02 	. .
	jp (iy)		;06aa	fd e9 	. .

;	DISPLAY H LOW NIBBLE IN HEX
DSH1:
	and 0x0f
	add a,"0"
	cp "9"+1
	jp m,_DISP
	add a,"A"-"9"-1
_DISP:
	ld c,a
	jp COIX



sub_06bbh:
	ld b,004h		;06bb	06 04 	. .
l06bdh:
	call RDCMOS		;06bd	cd cb 06 	. . .
	djnz l06bdh		;06c0	10 fb 	. .
	ld a,c			;06c2	79 	y
	cp 00fh		;06c3	fe 0f 	. .
	ld a,042h		;06c5	3e 42 	> B
	ret nz			;06c7	c0 	.
	ld a,0c0h		;06c8	3e c0 	> .
	ret			;06ca	c9 	.

;
;CMOS RAM OR CLOCK READ/WRITE
;  D=ADDRESS 0-63
;  D6,7=0 IF CMOS RAM
;  D6=1 IF CLOCK
;  D7=0 IF NATIONAL
;  D7=1 IF OKIDATA
;  C0-3=DATA TO BE WRITTEN
;  C4-7=0
;  ON EXIT C=DATA (IF READ)
;LEAVES WITH INTERRUPT OFF
;(A,C,D,E)
;
; Addresses:
; 	System Initialization
; 	50 Hz Flag
; 	Time Initialization
; 	Date Initialization
; 	Bell Flag
; 	Volume
; 	Printer Baud
; 	Communications Baud
; 	Key Sound
; 	Brightness High
; 	Brightness Low
; 	Year Low
; 	Year High
; 	Shift Lock Flag
; 	Next Alarm - Month
; 	Next Alarm - Day High
; 	Next Alarm - Day Low
; 	Next Alarm - Hour High
; 	Next Alarm - Hour Low
; 	Next Alarm - Minute High
; 	Next Alarm - Minute Low
; 	Next Alarm - Alarm Number
; 	Alarm Type - Next execution mode
RDCMOS:
	ld e,00fh		;06cb	1e 0f 	. .
	jr l06d1h		;06cd	18 02 	. .
sub_06cfh:
	ld e,000h		;06cf	1e 00 	. .
l06d1h:
	ld a,0cfh		;06d1	3e cf 	> .
	push de			;06d3	d5 	.
	di			;06d4	f3 	.
	out (SPIOA),a		;06d5	d3 f9 	. .
	ld a,e			;06d7	7b 	{
	out (SPIOA),a		;06d8	d3 f9 	. .
	ld a,d			;06da	7a 	z
	rlca			;06db	07 	.
	rlca			;06dc	07 	.
	rlca			;06dd	07 	.
	rlca			;06de	07 	.
	ld d,a			;06df	57 	W
	and 0f0h		;06e0	e6 f0 	. .
	or c			;06e2	b1 	.
	out (DPIOA),a		;06e3	d3 f8 	. .
	ld a,0b0h		;06e5	3e b0 	> .
	bit 0,e		;06e7	cb 43 	. C
	jr z,l06edh		;06e9	28 02 	( .
	set 2,a		;06eb	cb d7 	. .
l06edh:
	bit 2,d		;06ed	cb 52 	. R
	jr z,l06f9h		;06ef	28 08 	( .
	xor 018h		;06f1	ee 18 	. .
	bit 3,d		;06f3	cb 5a 	. Z
	jr z,l06f9h		;06f5	28 02 	( .
	xor 004h		;06f7	ee 04 	. .
l06f9h:
	ld c,a			;06f9	4f 	O
	ld a,d			;06fa	7a 	z
	and 003h		;06fb	e6 03 	. .
	or c			;06fd	b1 	.
	out (DPIOB),a		;06fe	d3 fa 	. .
	res 5,a		;0700	cb af 	. .
	out (DPIOB),a		;0702	d3 fa 	. .
	push af			;0704	f5 	.
	in a,(DPIOA)		;0705	db f8 	. .
	and 00fh		;0707	e6 0f 	. .
	ld c,a			;0709	4f 	O
	pop af			;070a	f1 	.
	set 5,a		;070b	cb ef 	. .
	out (DPIOB),a		;070d	d3 fa 	. .
	pop de			;070f	d1 	.
	ret			;0710	c9 	.

;	Loads the 4 bytes (8x8k pages) virtual data map pointed by HL
;	Pages are 8K. Ram is 16 pages 0000->1111. Logical are 4 pages, 000->111
;	Format of command is 'lll0pppp' => logical page lll shows physical pppp
MAPMEM:
	ld bc,DMAP		; b = 0, c = DMAP
MAPMEMLOOP:
	rld				; High nibble of (HL)
	out (c),a
	ld a,b
	add a,0x20		; Next logical page
	ld b,a
	rrd				; Low nibble of (HL)
	out (c),a
	ld a,b
	add a,0x20		; Next logical page
	ld b,a
	inc hl
	or a			; All pages done?
	jr nz,MAPMEMLOOP
	jp (ix)

sub_072ah: ; Print HL?
	pop iy		;072a	fd e1 	. .
DISP_HL:
	ld c,(hl)
	ld b,c
	res 7,c			; clear bit 7
	inc hl
	CALLIX COIX
	bit 7,b		;	bit 7 => end of string
	jr z,DISP_HL
	jp (iy)
	pop ix		;073e	dd e1 	. .

;
;	MAKE THE SOUND SPECIFIED BY THE 14 BYTE TABLE
;  	POINTED TO BY H-L (R14 OF THE SOUND GENERATOR
;  	IS FIRST, R0 LAST)
;
SOUND:
	ld a,0cfh
	di
	; PIO port A: Control mode 3, all ports as output
	out (SPIOA),a
	xor a
	out (SPIOA),a

	; no-op, strobe 138
	ld a,0xe3 ; 8910 data load
	out (DPIOB),a

	ld bc,0x0ef8		; b=14, c=0xf8
_L1:
	dec b
	out (c),b
	inc b
	; 8910 addr load & strobe
	ld a,0xc3
	out (DPIOB),a
	ld a,0xe3
	out (DPIOB),a
	; send data
	outi
	; 8910 data load & strobe
	ld a,0xe7
	out (DPIOB),a
	ld a,0xc7
	out (DPIOB),a
	ld a,0xe7
	out (DPIOB),a
	jr nz,_L1
	jp (ix)

sub_076dh:
	pop ix		;076d	dd e1 	. .
l076fh:
	CALLIY l081ah		;0773	c3 1a 08 	. . .
	jr z,l0781h		;0776	28 09 	( .
	xor a			;0778	af 	.
	ld (KEYFLG),a		;0779	32 79 fd 	2 y .
	ld a,(KEYCOD)		;077c	3a 78 fd 	: x .
	jr l078ch		;077f	18 0b 	. .
l0781h:
	CALLIY l0790h		;0785	c3 90 07 	. . .
	jr z,l076fh		;0788	28 e5 	( .
	in a,(DCOMM)		;078a	db f0 	. .
l078ch:
	jp (ix)		;078c	dd e9 	. .
sub_078eh:
	pop iy		;078e	fd e1 	. .
l0790h:
	ld a,010h		;0790	3e 10 	> .
	out (SCOMM),a		;0792	d3 f1 	. .
	in a,(SCOMM)		;0794	db f1 	. .
	and 001h		;0796	e6 01 	. .
	jp (iy)		;0798	fd e9 	. .
XXXKEYSTS:
	pop ix		;079a	dd e1 	. .
l079ch:
	ld iy,l07a2h		;079c	fd 21 a2 07 	. ! . .
	jr l081ah		;07a0	18 78 	. x
l07a2h:
	jr nz,l07aah		;07a2	20 06 	  .
	ld iy,l07aah		;07a4	fd 21 aa 07 	. ! . .
	jr l0790h		;07a8	18 e6 	. .
l07aah:
	jp (ix)		;07aa	dd e9 	. .

;	COMM/PRTR INPUT ROUTINE
; 	 EITHER OR BOTH PORTS MAY BE SELECTED
; 	 AS SPECIFIED BY THE S COMMAND.
;	 FETCHES COMM. PORT 1ST.
; 	 RETURNS '?' IF NEITHER SELECTED
;
CIPI:
	ld a,(KEYFLG)		;07ac	3a 79 fd 	: y .
	or a			;07af	b7 	.
	jp nz,ABORT		;07b0	c2 20 06 	.   .
	ld a,(OUTSEL)		;07b3	3a 85 fd 	: . .
	and 011h		;07b6	e6 11 	. .
	jp z,l0615h		;07b8	ca 15 06 	. . .
	bit 0,a		;07bb	cb 47 	. G
	jr z,l07c7h		;07bd	28 08 	( .
	call sub_078eh		;07bf	cd 8e 07 	. . .
	jr z,l07c7h		;07c2	28 03 	( .
	in a,(DCOMM)		;07c4	db f0 	. .
	ret			;07c6	c9 	.
l07c7h:
	ld a,(OUTSEL)		;07c7	3a 85 fd 	: . .
	bit 4,a		;07ca	cb 67 	. g
	jr z,CIPI		;07cc	28 de 	( .
	ld a,010h		;07ce	3e 10 	> .
	out (SPRTR),a		;07d0	d3 f3 	. .
	in a,(SPRTR)		;07d2	db f3 	. .
	and 001h		;07d4	e6 01 	. .
	jr z,CIPI		;07d6	28 d4 	( .
	in a,(DPRTR)		;07d8	db f2 	. .
	ret			;07da	c9 	.
sub_07dbh:
	pop ix		;07db	dd e1 	. .
	jr l07ebh		;07dd	18 0c 	. .

SPACE:
	ld c,' '
CO:		;	Character Out
	pop ix
COIX:
	ld a,i		;07e3	ed 57 	. W
	or a			;07e5	b7 	.
	jr z,l07f2h		;07e6	28 0a 	( .
	call DISPLY
l07ebh:
	ld a,(OUTSEL)		;07eb	3a 85 fd 	: . .
	bit 4,a		;07ee	cb 67 	. g
	jr z,l0801h		;07f0	28 0f 	( .
l07f2h:
	ld a,010h		;07f2	3e 10 	> .
	out (SPRTR),a		;07f4	d3 f3 	. .
	in a,(SPRTR)		;07f6	db f3 	. .
	and 024h		;07f8	e6 24 	. $
	cp 024h		;07fa	fe 24 	. $
	jr nz,l07f2h		;07fc	20 f4 	  .
	ld a,c			;07fe	79 	y
	out (DPRTR),a		;07ff	d3 f2 	. .
l0801h:
	ld a,(OUTSEL)		;0801	3a 85 fd 	: . .
	bit 0,a		;0804	cb 47 	. G
	jr z,l0817h		;0806	28 0f 	( .
l0808h:
	ld a,010h		;0808	3e 10 	> .
	out (SCOMM),a		;080a	d3 f1 	. .
	in a,(SCOMM)		;080c	db f1 	. .
	and 024h		;080e	e6 24 	. $
	cp 024h		;0810	fe 24 	. $
	jr nz,l0808h		;0812	20 f4 	  .
	ld a,c			;0814	79 	y
	out (DCOMM),a		;0815	d3 f0 	. .
l0817h:
	ld a,c			;0817	79 	y
	jp (ix)

l081ah:
	ld a,i		;081a	ed 57 	. W
	or a			;081c	b7 	.
	jr z,l0823h		;081d	28 04 	( .
	ld a,(KEYFLG)		;081f	3a 79 fd 	: y .
	or a			;0822	b7 	.
l0823h:
	jp (iy)		;0823	fd e9 	. .


; TERMINAL EMULATOR

TERMNL:
	xor a			; Clear input
	ld (KEYFLG),a
_T0:
	call sub_078eh		;0829	cd 8e 07 	. . .
	jr z,_T1		;082c	28 08 	( .
	in a,(DCOMM)
	and 0x7f
	ld c,a
	call CO
_T1:
	ld a,(KEYFLG)		;0836	3a 79 fd 	: y .
	or a			;
	jr z,_T0		; No key? loop

	ld a,(KEYCOD)		;083c	3a 78 fd 	: x .
	ld c,a			;083f	4f 	O
	cp CTRLLF
	jr nz,_T2		;0842	20 0c 	  .
	di			;0844	f3 	.
	ld sp,STACK_BASE		;0845	31 00 fe 	1 . .
	xor a			;0848	af 	.
	ld (KEYFLG),a		;0849	32 79 fd 	2 y .
	ei			;084c	fb 	.
	jp MNTR
_T2:
	ld a,010h		;0850	3e 10 	> .
	out (SCOMM),a		;0852	d3 f1 	. .
	in a,(SCOMM)		;0854	db f1 	. .
	and 004h		;0856	e6 04 	. .
	jr z,_T0		;0858	28 cf 	( .
	di			;085a	f3 	.
	xor a			;085b	af 	.
	ld (KEYFLG),a		;085c	32 79 fd 	2 y .
	ei			;085f	fb 	.
	ld a,c			;0860	79 	y
	out (DCOMM),a		;0861	d3 f0 	. .
	jr _T0

;DPIOB:	equ 0fah	; PIO PORT B DATA
			; BO-1 = 5101 A4-5
			; B2-4 = OPERATION SELECT
			; 0= 8910 ADDR LOAD
			; 1= 8910 DATA LOAD
			; 2= 5832 WRITE
			; 3= 5832 READ
			; 4= 5101 WRITE
			; 5 = 5101 READ
			; 6 = LATCH LOAD
			; 7 = NO-OP
			; B5 = /1138 OPERATION STROBE
			; B6 = /KEYBOARD DATA IN
			; B7 = /KEYBOARD CLOCK OUT

;
;|------------------------------|
;| 	DISK SUBROUTINES	|
;|------------------------------|
;
;GENERAL FLOPPY HANDLER
;(A,B,C,D,E,H,L)
;READS A TABLE TO DO DISK OPERATIONS
;  FIRST BYTE
;	0-3 = # OF BYTES TO SEND TO 765
;	4 = 1 IF DMA TO BE ACTIVATED
;	5 = 1 IF INTERRUPT IS TO BE PROCESSED
;	6 = 1 IF WRITE TO DISK, ELSE READ
;	7 = 1 IF ST3 IS TO BE READ
;  IF DMA IS NEEDED, THE NEXT TO BYTES ARE
;    THE START LOCATION IN MEMORY
;  NEXT BYTES ARE TRANSFERRED TO THE 765
; IF ERROR, RETURN ADDRESS IN X REG.
;  (USUALLY CONTAINS ADDRESS OF ERROR ROUTINE)
;

DISKOP:
	push hl
	ld hl,(MTRCNT)		; Floppy motor timer, NZ == motor on
	ld a,h
	or l
	push af
	ld a,(LSTATE)
	set 0,a			; Set 'motor on' bit
	ld bc,0fbh*256+DPIOB
	push af

	ld a,0cfh
	di
	out (SPIOA),a		; PIO Port A: bit mode & every line as output
	xor a
	out (SPIOA),a		; PIO Port A: every line as output
	pop af
	out (DPIOA),a		; Motor on & graphics enable

	; DPIOB: Latch load & operation stobe
	out (c),b
	res 5,b
	out (c),b
	set 5,b
	out (c),b

	ei

	; Reset motor timer to 300ms
	ld hl,MTRTIM
	ld (MTRCNT),hl

	pop af			;0891	f1 	.
	jr nz,_DISKOP2		; Motor on?
	scf			;0894	37 	7
	ld de,00100h
_WAITSPEED:
	; Wait for motor to be at speed
	ld hl,(MTRCNT)
	sbc hl,de
	jp p,_WAITSPEED
_DISKOP2:
	pop hl			;08a0	e1 	.
	ld d,h			;08a1	54 	T
	ld e,l			;08a2	5d 	]
	bit 4,(hl)		; DMA bit set?
	jr z,_DMA_DONE

	; Configure DMA
	ld a,044h		; Single mode, addr increment, no autoinit, Write, CH0
	bit 6,(hl)		; Do we write to disk?
	jr z,_DMASETUP
	ld a,048h		; Single mode, addr increment, no autoinit, Read, CH0
_DMASETUP:
	out (DMAWMR),a		;08af	d3 eb 	. .

	; Check second bit for addr in memory
	push hl			;08b1	e5 	.
	inc hl			;08b2	23 	#
	ld c,0e0h		;08b3	0e e0 	. .
	outi			;08b5	ed a3 	. .
	outi			;08b7	ed a3 	. .
	ld c,0e1h		;08b9	0e e1 	. .
	outi			;08bb	ed a3 	. .
	outi			;08bd	ed a3 	. .
	ld a,000h		;08bf	3e 00 	> .
	out (DMAWSM),a		;08c1	d3 ea 	. .
	dec hl			;08c3	2b 	+
	ex de,hl			;08c4	eb 	.
	pop hl			;08c5	e1 	.
_DMA_DONE:
	push hl			;08c6	e5 	.
	ld a,(hl)			;08c7	7e 	~
	and 00fh		;08c8	e6 0f 	. .
	ld b,a			;08ca	47 	G
	ld h,d			;08cb	62 	b
	ld l,e			;08cc	6b 	k
l08cdh:
	inc hl			;08cd	23 	#
	ld c,(hl)			;08ce	4e 	N
	push bc			;08cf	c5 	.
	call SEND_FDC_CMD		;08d0	cd 8f 0a 	. . .
	pop bc			;08d3	c1 	.
	ld a,001h		;08d4	3e 01 	> .
	jr nz,l08dch		;08d6	20 04 	  .
	pop bc			;08d8	c1 	.
	pop bc			;08d9	c1 	.
	jp (ix)		;08da	dd e9 	. .
l08dch:
	djnz l08cdh		;08dc	10 ef 	. .
	pop hl			;08de	e1 	.
	bit 5,(hl)		;08df	cb 6e 	. n
	jr nz,l08f2h		;08e1	20 0f 	  .
	bit 7,(hl)		;08e3	cb 7e 	. ~
	ret z			;08e5	c8 	.
	call READ_FDC_REG		;08e6	cd a5 0a 	. . .
	ld (ST3),a		;08e9	32 1c fd 	2 . .
	ld a,002h		;08ec	3e 02 	> .
	ret nz			;08ee	c0 	.
	pop bc			;08ef	c1 	.
	jp (ix)		;08f0	dd e9 	. .
l08f2h:
	ld b,002h		;08f2	06 02 	. .
l08f4h:
	ld hl,0a000h		;08f4	21 00 a0 	! . .
l08f7h:
	ld a,(FPYFLG)		;08f7	3a 1e fd 	: . .
	or a			;08fa	b7 	.
	jr nz,l0909h		;08fb	20 0c 	  .
	dec hl			;08fd	2b 	+
	ld a,h			;08fe	7c 	|
l08ffh:
	or l			;08ff	b5 	.
	jr nz,l08f7h		;0900	20 f5 	  .
	djnz l08f4h		;0902	10 f0 	. .
	ld a,0ffh		;0904	3e ff 	> .
	pop bc			;0906	c1 	.
	jp (ix)		;0907	dd e9 	. .
l0909h:
	xor a			;0909	af 	.
	ld (FPYFLG),a		;090a	32 1e fd 	2 . .
	ret			;090d	c9 	.

ERROR:
	call sub_0913h		;090e	cd 13 09 	. . .
	jr l092ah		;0911	18 17 	. .

sub_0913h:
	ld a,(ST0)		;0913	3a 15 fd 	: . .
	ld d,a			;0916	57 	W
	ld a,(ST1)		;0917	3a 16 fd 	: . .
	ld e,a			;091a	5f 	_
	push hl			;091b	e5 	.
	call sub_0685h		;091c	cd 85 06 	. . .
	pop hl			;091f	e1 	.
	ld a,(ERRCNT)		;0920	3a 1d fd 	: . .
	inc a			;0923	3c 	<
	ld (ERRCNT),a		;0924	32 1d fd 	2 . .
	cp 017h		;0927	fe 17 	. .
	ret m			;0929	f8 	.
l092ah:
	jp GOMON		;092a	c3 2f 06 	. / .
sub_092dh:
	push hl			;092d	e5 	.
	ld hl,FPYTBL		;092e	21 0a 0f 	! . .
	ld de,0fd00h		;0931	11 00 fd 	. . .
	ld bc,0015h		;0934	01 15 00 	. . .
	ldir		;0937	ed b0 	. .
	pop hl			;0939	e1 	.
	bit 6,l		;093a	cb 75 	. u
	jr z,l0943h		;093c	28 05 	( .
	ld a,001h		;093e	3e 01 	> .
	ld (RHD),a		;0940	32 08 fd 	2 . .
l0943h:
	ld a,l			;0943	7d 	}
	rlca			;0944	07 	.
	rlca			;0945	07 	.
	rlca			;0946	07 	.
	rlca			;0947	07 	.
	and 005h		;0948	e6 05 	. .
	ld (RDRV),a		;094a	32 06 fd 	2 . .
	ld (SDRV),a		;094d	32 13 fd 	2 . .
	res 2,a		;0950	cb 97 	. .
	ld (HDRV),a		;0952	32 10 fd 	2 . .
	ld a,(HOMES)		;0955	3a 1f fd 	: . .
	bit 4,l		;0958	cb 65 	. e
	jr nz,l0964h		;095a	20 08 	  .
	bit 0,a		;095c	cb 47 	. G
	set 0,a		;095e	cb c7 	. .
	jr nz,l0980h		;0960	20 1e 	  .
	jr l096ah		;0962	18 06 	. .
l0964h:
	bit 1,a		;0964	cb 4f 	. O
	set 1,a		;0966	cb cf 	. .
	jr nz,l0980h		;0968	20 16 	  .
l096ah:
	ld (HOMES),a		;096a	32 1f fd 	2 . .
	push hl			;096d	e5 	.
	ld hl,RCLTBL		;096e	21 0e fd 	! . .
	push hl			;0971	e5 	.
	call DISKOP
	ld hl,SEKTBL		;0975	21 11 fd 	! . .
	call DISKOP
	pop hl			;097b	e1 	.
	call DISKOP
	pop hl			;097f	e1 	.
l0980h:
	ld a,l			;0980	7d 	}
	and 00fh		;0981	e6 0f 	. .
	ld (RCRD),a		;0983	32 09 fd 	2 . .
	ld a,h			;0986	7c 	|
	ld (NCN),a		;0987	32 14 fd 	2 . .
	ld (RDCYL),a		;098a	32 07 fd 	2 . .
	ld hl,SEKTBL		;098d	21 11 fd 	! . .
	call DISKOP
	ld hl,TYPE		;0993	21 00 fd 	! . .
	ret			;0996	c9 	.

;READ A FLOPPY SECTOR, WHERE
;H=CYLINDER, L7=1 IF 96 TPI,
;L6=HEAD, L4=DRIVE, L0-3=SECTOR (1-10)
FPREAD:
	push hl
	call sub_092dh		;0998	cd 2d 09 	. - .
	call DISKOP
	pop hl
	ret

;WRITE A FLOPPY SECTOR, WHERE
;H=CYLINDER, L7=1 IF 96 TPI,
;L6=HEAD, L4=DRIVE, L0-3=SECTOR (1-10)f
FPWRIT:
	push hl
	call sub_092dh		;09a1	cd 2d 09 	. - .
	ld (hl),0x79
	ld a,DBL*40H+5
	ld (CMMD),a		;09a8	32 05 fd 	2 . .
	call DISKOP
	pop hl
DLY2MS:
	ld b,13
	call DELAY
	ret



SELFMT:
	xor a			;09b5	af 	.
	bit 4,l		;09b6	cb 65 	. e
	jr z,l09bch		;09b8	28 02 	( .
	set 7,a		;09ba	cb ff 	. .
l09bch:
	bit 0,l		;09bc	cb 45 	. E
	jr z,l09c2h		;09be	28 02 	( .
	set 4,a		;09c0	cb e7 	. .
l09c2h:
	ld l,a			;09c2	6f 	o
	ld h,000h		;09c3	26 00 	& .
l09c5h:
	ex de,hl			;09c5	eb 	.
	ld hl,STACK_BASE		;09c6	21 00 fe 	! . .
	ld b,00ah		;09c9	06 0a 	. .
l09cbh:
	ld (hl),d			;09cb	72 	r
	inc hl			;09cc	23 	#
	xor a			;09cd	af 	.
	bit 6,e		;09ce	cb 73 	. s
	jr z,l09d4h		;09d0	28 02 	( .
	ld a,001h		;09d2	3e 01 	> .
l09d4h:
	ld (hl),a			;09d4	77 	w
	inc hl			;09d5	23 	#
	ld a,00bh		;09d6	3e 0b 	> .
	sub b			;09d8	90 	.
	ld (hl),a			;09d9	77 	w
	inc hl			;09da	23 	#
	ld (hl),002h		;09db	36 02 	6 .
	inc hl			;09dd	23 	#
	djnz l09cbh		;09de	10 eb 	. .
	ex de,hl			;09e0	eb 	.
	push hl			;09e1	e5 	.
	call sub_092dh		;09e2	cd 2d 09 	. - .
	push hl			;09e5	e5 	.
	ex de,hl			;09e6	eb 	.
	ld hl,FORMAT		;09e7	21 23 0f 	! # .
	ld bc,0000bh		;09ea	01 0b 00 	. . .
	ldir		;09ed	ed b0 	. .
	ld a,(SDRV)		;09ef	3a 13 fd 	: . .
	ld (RDRV),a		;09f2	32 06 fd 	2 . .
	pop hl			;09f5	e1 	.
	call ABTTST		;09f6	cd 1c 06 	. . .
	ld ix,ERROR
	call DISKOP
	pop hl			;0a00	e1 	.
	call DLY2MS
	inc h			;0a04	24 	$
	ld a,h			;0a05	7c 	|
	cp 028h		;0a06	fe 28 	. (
	jr nz,l09c5h		;0a08	20 bb 	  .
	bit 7,l		;0a0a	cb 7d 	. }
	jr z,l0a12h		;0a0c	28 04 	( .
	cp 050h		;0a0e	fe 50 	. P
	jr nz,l09c5h		;0a10	20 b3 	  .
l0a12h:
	bit 6,l		;0a12	cb 75 	. u
	set 6,l		;0a14	cb f5 	. .
	ld h,000h		;0a16	26 00 	& .
	jr z,l09c5h		;0a18	28 ab 	( .
	ret			;0a1a	c9 	.

BOOT:
	ld c,000h		;0a1b	0e 00 	. .
	call sub_0a80h		;0a1d	cd 80 0a 	. . .
	call l0909h		;0a20	cd 09 09 	. . .
	ld hl,SPECIFY		;0a23	21 1f 0f 	! . .
	call DISKOP
	ld a,003h		;0a29	3e 03 	> .
l0a2bh:
	ld (BOTSTK),sp		;0a2b	ed 73 7c fd 	. s | .
l0a2fh:
	ld (BOOTRY),a		;0a2f	32 7b fd 	2 { .
	ld ix,l0a56h		;0a32	dd 21 56 0a 	. ! V .
	ld hl,1		;0a36	21 01 00 	! . .
	call FPREAD
	ld hl,NOSYS_MSG		;0a3c	21 84 0f 	! . .
	ld a,(ST0)		;0a3f	3a 15 fd 	: . .
	and 0c0h		;0a42	e6 c0 	. .
	jr nz,l0a56h		;0a44	20 10 	  .
	ld a,(0fe03h)		;0a46	3a 03 fe 	: . .
	cp 0a7h		;0a49	fe a7 	. .

		;	Jumps to loaded code
	jp z,STACK_BASE		;0a4b	ca 00 fe 	. . .
	ld a,(BOOTRY)		;0a4e	3a 7b fd 	: { .
	dec a			;0a51	3d 	=
	jr nz,l0a2fh		;0a52	20 db 	  .
	jr l0a6bh		;0a54	18 15 	. .
l0a56h:
	ld c,008h		;0a56	0e 08 	. .
	call sub_0a80h		;0a58	cd 80 0a 	. . .
	call READ_FDC_REG		;0a5b	cd a5 0a 	. . .
	ld sp,(BOTSTK)		;0a5e	ed 7b 7c fd 	. { | .
	ld a,(BOOTRY)		;0a62	3a 7b fd 	: { .
	dec a			;0a65	3d 	=
	jr nz,l0a2bh		;0a66	20 c3 	  .
	ld hl,NODISK_MSG	;0a68	21 66 0f 	! f .
l0a6bh:
	call sub_072ah		;0a6b	cd 2a 07 	. * .

;	Launch terminal
GOTERM:
	ld hl,TERMINAL_MSG	;0a6e	21 97 0f 	! . .
	call sub_072ah		;0a71	cd 2a 07 	. * .
	di			;0a74	f3 	.
	xor a			;0a75	af 	.
	ld (KEYFLG),a		;0a76	32 79 fd 	2 y .
	ld sp,STACK_BASE		;0a79	31 00 fe 	1 . .
	ei			;0a7c	fb 	.
	jp TERMNL

sub_0a80h:
	ld b,00ah		;0a80	06 0a 	. .
l0a82h:
	push bc			;0a82	c5 	.
	call READ_FDC_REG		;0a83	cd a5 0a 	. . .
	pop bc			;0a86	c1 	.
	djnz l0a82h		;0a87	10 f9 	. .
	call SEND_FDC_CMD		;0a89	cd 8f 0a 	. . .
	jp READ_FDC_REG		;0a8c	c3 a5 0a 	. . .

SEND_FDC_CMD: ; = W765 in MONX.ASM
	ld b,01fh
l0a91h:
	djnz l0a91h		; Wait ~100us
	ld b,00ah		; b = 10, check 10 times
l0a95h:
	; Wait for FDC to be ready for data register set
	in a,(SFLPY)		; Read register
	and 0c0h		; Only check DIO & RQM bits
	cp 080h			;
	jr z,l0aa0h		; DIO=0, RQM=1, continue
	djnz l0a95h		; b=b-1, if b!=0, jump
	; Stop if not ready after 10 tries
	ret			; 10 fails, return (PC = (SP+))
l0aa0h:
	or a			; success, z = 0 ?
	ld a,c			;
	out (DFLPY),a		; c -> FDC
	ret			;


READ_FDC_REG:
	ld b,01fh
l0aa7h:
	djnz l0aa7h		; Wait ~100us
	ld b,00ah		; b = 10, check 10 times
l0aabh:
	; Wait for FDC to be ready for data register read
	in a,(SFLPY)		; Read register
	and 0c0h		; Only check DIO & RQM
	cp 0c0h
	jr z,l0ab6h		; DQM=1, DIO=1, data ready to be read
	djnz l0aabh
	; Stop if not ready after 10 tries
	ret			; error, z != 0
l0ab6h:
	or a			; success, z = 0
	in a,(DFLPY)		; a <- data register
	ret



	;	Prints the char in C, interpreting escape codes
DISPLY:
	push hl
	push de
	push bc
	push af
l0abeh:
	ld hl,(CURSOR)		;0abe	2a 22 fd 	* " .
	ld a,(LINOFS)		;0ac1	3a 77 fd 	: w .
	add a,l			;0ac4	85 	.
	cp 018h		;0ac5	fe 18 	. .
	jr c,l0acbh		;0ac7	38 02 	8 .
	sbc a,018h		;0ac9	de 18 	. .
l0acbh:
	ld l,a			;0acb	6f 	o
	ld a,(DSPCYC)
	or a
	jp nz,ESCAPE	; We're in an ESC sequence
	ld a,c
	and 0x60		; zero if ascii less than ' '
	jr z,CONTROL	; Handle control chars

	ld a,c
	cp 040h		;0ad9	fe 40 	. @
	jr c,l0af3h		;0adb	38 16 	8 .
	cp 061h		;0add	fe 61 	. a
	jr nc,l0af3h		;0adf	30 12 	0 .
	and 03fh		;0ae1	e6 3f 	. ?
	push af			;0ae3	f5 	.
	ld de,00f37h		;0ae4	11 37 0f 	. 7 .
	ex de,hl			;0ae7	eb 	.
	ld a,(CURCHR)		;0ae8	3a 25 fd 	: % .
	ld c,a			;0aeb	4f 	O
	ld b,000h		;0aec	06 00 	. .
	add hl,bc			;0aee	09 	.
	ld a,(hl)			;0aef	7e 	~
	pop bc			;0af0	c1 	.
	ex de,hl			;0af1	eb 	.
	or b			;0af2	b0 	.
l0af3h:
	ld e,a			;0af3	5f 	_
	ld a,l			;0af4	7d 	}
	or 0e0h		;0af5	f6 e0 	. .
	out (SDSPY),a
	ld b,h			;0af9	44 	D
	ld c,DDSPY
	out (c),e		;0afc	ed 59 	. Y
	and 0dfh		;0afe	e6 df 	. .
	out (SDSPY),a		;0b00	d3 ee 	. .
	and 01fh		;0b02	e6 1f 	. .
	ld l,a			;0b04	6f 	o
	ld a,(CURATT)		;0b05	3a 24 fd 	: $ .
	out (c),a		;0b08	ed 79 	. y
	ld a,h			;0b0a	7c 	|
	cp 04fh		;0b0b	fe 4f 	. O
	jr nz,NEXTCOL		;0b0d	20 08 	  .
l0b0fh:
	xor a			;0b0f	af 	.
	ld (CURSORCOL),a		;0b10	32 23 fd 	2 # .
	ld c,00ah		;0b13	0e 0a 	. .
	jr l0abeh		;0b15	18 a7 	. .
NEXTCOL:
	inc a			;0b17	3c 	<
SETCOL:
	ld h,a			;0b18	67 	g
	ld (CURSORCOL),a		;0b19	32 23 fd 	2 # .
l0b1ch:
	ld a,0ach		;0b1c	3e ac 	> .
	out (SDSPY),a		;0b1e	d3 ee 	. .
	ld a,h			;0b20	7c 	|
	inc a			;0b21	3c 	<
	out (DDSPY),a		;0b22	d3 fe 	. .
	ld a,0adh		;0b24	3e ad 	> .
	out (SDSPY),a		;0b26	d3 ee 	. .
	ld a,l			;0b28	7d 	}
	and 01fh		;0b29	e6 1f 	. .
	out (DDSPY),a		;0b2b	d3 fe 	. .
CLEARESC:
	xor a
STOREESC:
	ld (DSPCYC),a		;0b2e	32 76 fd 	2 v .
l0b31h:
	pop af			;0b31	f1 	.
	pop bc			;0b32	c1 	.
	pop de			;0b33	d1 	.
	pop hl			;0b34	e1 	.
	ret			;0b35	c9 	.



	;	We print ESC sequences and control chars
CONTROL:
	ld a,c
	cp 0x1b			; ESC
	jr nz,CONTROL1
	ld a,1
	jp STOREESC

	;	Single char control
CONTROL1:
	cp 8			; Backspace
	jr nz,l0b4ch		;0b42	20 08 	  .
	ld a,h			;0b44	7c 	|
	or a			;0b45	b7 	.
	jr z,CLEARESC
	dec a
	jp SETCOL
l0b4ch:
	cp 00dh		;0b4c	fe 0d 	. .
	jr nz,l0b54h		;0b4e	20 04 	  .
	xor a
	jp SETCOL
l0b54h:
	cp 00ah		;0b54	fe 0a 	. .
	jr nz,l0b91h		;0b56	20 39 	  9
	ld a,(CURSORROW)		;0b58	3a 22 fd 	: " .
	cp 017h		;0b5b	fe 17 	. .
	jr z,l0b6dh		;0b5d	28 0e 	( .
	inc a			;0b5f	3c 	<
	ld (CURSORROW),a		;0b60	32 22 fd 	2 " .
	inc l			;0b63	2c 	,
	ld a,l			;0b64	7d 	}
	cp 018h		;0b65	fe 18 	. .
	jr nz,l0b1ch		;0b67	20 b3 	  .
	xor a			;0b69	af 	.
	ld l,a			;0b6a	6f 	o
	jr l0b1ch		;0b6b	18 af 	. .
l0b6dh:
	ld a,0abh		;0b6d	3e ab 	> .
	out (SDSPY),a		;0b6f	d3 ee 	. .
	out (DDSPY),a		;0b71	d3 fe 	. .
	ld a,(LINOFS)		;0b73	3a 77 fd 	: w .
	inc a
	cp 24
	jr nz,l0b7ch		;0b79	20 01 	  .
	xor a			;0b7b	af 	.
l0b7ch:
	ld (LINOFS),a		;0b7c	32 77 fd 	2 w .
	ld a,l			;0b7f	7d 	}
	inc a			;0b80	3c 	<
	inc l			;0b81	2c 	,
	cp 24
	jr nz,l0b88h		;0b84	20 02 	  .
	ld l,000h		;0b86	2e 00 	. .
l0b88h:
	call sub_0c7ah		;0b88	cd 7a 0c 	. z .
	ld a,(CURSORCOL)		;0b8b	3a 23 fd 	: # .
	ld h,a			;0b8e	67 	g
	jr l0b1ch		;0b8f	18 8b 	. .
l0b91h:
	cp 007h		;0b91	fe 07 	. .
	jr nz,l0ba3h		;0b93	20 0e 	  .
	ld hl,BEEP		;0b95	21 de 0f 	! . .
	CALLIX SOUND
	ei			;0b9f	fb 	.
	jp CLEARESC		;0ba0	c3 2d 0b 	. - .
l0ba3h:
	cp 01eh		;0ba3	fe 1e 	. .
	jr nz,l0bb2h		;0ba5	20 0b 	  .
l0ba7h:
	ld hl,1		;0ba7	21 01 00 	! . .
	ld (CURSORROW),hl		;0baa	22 22 fd 	" " .
	ld c,008h		;0bad	0e 08 	. .
	jp l0abeh		;0baf	c3 be 0a 	. . .
l0bb2h:
	cp 00bh		;0bb2	fe 0b 	. .
	jp z,l0bd7h		;0bb4	ca d7 0b 	. . .
	cp 00ch		;0bb7	fe 0c 	. .
	jp nz,l0b31h		;0bb9	c2 31 0b 	. 1 .
	jp l0c08h		;0bbc	c3 08 0c 	. . .


; ESC code management

ESCAPE:
	dec a			;0bbf	3d 	=
	jp nz,l0c55h		;0bc0	c2 55 0c 	. U .
	ld a,c			;0bc3	79 	y
	cp 03dh		;0bc4	fe 3d 	. =
	jr nz,l0bcfh		;0bc6	20 07 	  .
l0bc8h:
	ld hl,DSPCYC		;0bc8	21 76 fd 	! v .
	inc (hl)			;0bcb	34 	4
	jp l0b31h		;0bcc	c3 31 0b 	. 1 .
l0bcfh:
	cp 059h		;0bcf	fe 59 	. Y
	jr z,l0bc8h		;0bd1	28 f5 	( .
	cp 041h		;0bd3	fe 41 	. A
	jr nz,l0bebh		;0bd5	20 14 	  .
l0bd7h:
	ld a,(CURSORROW)		;0bd7	3a 22 fd 	: " .
	or a			;0bda	b7 	.
	jp z,CLEARESC		;0bdb	ca 2d 0b 	. - .
	dec a			;0bde	3d 	=
	dec l			;0bdf	2d 	-
	jp p,l0be5h		;0be0	f2 e5 0b 	. . .
	ld l,017h		;0be3	2e 17 	. .
l0be5h:
	ld (CURSORROW),a		;0be5	32 22 fd 	2 " .
	jp l0b1ch		;0be8	c3 1c 0b 	. . .
l0bebh:
	cp 042h		;0beb	fe 42 	. B
	jr nz,l0c04h		;0bed	20 15 	  .
	ld a,(CURSORROW)		;0bef	3a 22 fd 	: " .
	cp 017h		;0bf2	fe 17 	. .
	jp z,CLEARESC		;0bf4	ca 2d 0b 	. - .
	inc a			;0bf7	3c 	<
	inc l			;0bf8	2c 	,
	push af			;0bf9	f5 	.
	ld a,l			;0bfa	7d 	}
	cp 018h		;0bfb	fe 18 	. .
	jr nz,l0c01h		;0bfd	20 02 	  .
	ld l,000h		;0bff	2e 00 	. .
l0c01h:
	pop af			;0c01	f1 	.
	jr l0be5h		;0c02	18 e1 	. .
l0c04h:
	cp 043h		;0c04	fe 43 	. C
	jr nz,l0c12h		;0c06	20 0a 	  .
l0c08h:
	ld a,h			;0c08	7c 	|
	cp 04fh		;0c09	fe 4f 	. O
	jp z,l0b0fh		;0c0b	ca 0f 0b 	. . .
	inc a
	jp SETCOL
l0c12h:
	cp 044h		;0c12	fe 44 	. D
	jr nz,l0c1fh		;0c14	20 09 	  .
	ld a,h			;0c16	7c 	|
	or a			;0c17	b7 	.
	jp z,CLEARESC		;0c18	ca 2d 0b 	. - .
	dec a			;0c1b	3d 	=
	jp SETCOL
l0c1fh:
	cp 04ah		;0c1f	fe 4a 	. J
	jr nz,l0c43h		;0c21	20 20
	call sub_0c9fh		;0c23	cd 9f 0c 	. . .
	ld a,(CURSORROW)		;0c26	3a 22 fd 	: " .
	sbc a,019h		;0c29	de 19 	. .
	cpl			;0c2b	2f 	/
	ld b,a			;0c2c	47 	G
	jr l0c3eh		;0c2d	18 0f 	. .
l0c2fh:
	ld a,l			;0c2f	7d 	}
	and 01fh		;0c30	e6 1f 	. .
	inc a			;0c32	3c 	<
	cp 018h		;0c33	fe 18 	. .
	jr nz,l0c38h		;0c35	20 01 	  .
	xor a			;0c37	af 	.
l0c38h:
	ld l,a			;0c38	6f 	o
	push bc			;0c39	c5 	.
	call sub_0c7ah		;0c3a	cd 7a 0c 	. z .
	pop bc			;0c3d	c1 	.
l0c3eh:
	djnz l0c2fh		;0c3e	10 ef 	. .
	jp CLEARESC		;0c40	c3 2d 0b 	. - .
l0c43h:
	cp 04bh		;0c43	fe 4b 	. K
	jr nz,l0c4dh		;0c45	20 06 	  .
	call sub_0c9fh		;0c47	cd 9f 0c 	. . .
	jp l0b1ch		;0c4a	c3 1c 0b 	. . .
l0c4dh:
	cp 048h		;0c4d	fe 48 	. H
	jp z,l0ba7h		;0c4f	ca a7 0b 	. . .
	jp CLEARESC		;0c52	c3 2d 0b 	. - .
l0c55h:
	dec a			;0c55	3d 	=
	jr nz,l0c6bh		;0c56	20 13 	  .
	ld a,003h		;0c58	3e 03 	> .
	ld (DSPCYC),a		;0c5a	32 76 fd 	2 v .
	ld a,c			;0c5d	79 	y
	sbc a,020h		;0c5e	de 20 	.
	cp 018h		;0c60	fe 18 	. .
	jp p,l0b31h		;0c62	f2 31 0b 	. 1 .
	ld (CURSORROW),a		;0c65	32 22 fd 	2 " .
	jp l0b31h		;0c68	c3 31 0b 	. 1 .
l0c6bh:
	dec a			;0c6b	3d 	=
	jp nz,CLEARESC		;0c6c	c2 2d 0b 	. - .
	ld a,c			;0c6f	79 	y
	sbc a,020h		;0c70	de 20 	.
	cp 80			; 80 columns
	jp p,CLEARESC
	jp SETCOL
sub_0c7ah:
	ld h,000h		;0c7a	26 00 	& .
	ld a,l			;0c7c	7d 	}
	or 0c0h		;0c7d	f6 c0 	. .
	ld l,a			;0c7f	6f 	o
	ld de,0020h		;0c80	11 20 00 	.   .
	ld bc,04feeh		;0c83	01 ee 4f 	. . O
	push bc			;0c86	c5 	.
	out (c),l		;0c87	ed 69 	. i
	ld c,DDSPY
l0c8bh:
	out (c),d		;0c8b	ed 51 	. Q
	djnz l0c8bh		;0c8d	10 fc 	. .
	out (c),d		;0c8f	ed 51 	. Q
	pop bc			;0c91	c1 	.
	set 5,l		;0c92	cb ed 	. .
	out (c),l		;0c94	ed 69 	. i
	ld c,DDSPY
l0c98h:
	out (c),e		;0c98	ed 59 	. Y
	djnz l0c98h		;0c9a	10 fc 	. .
	out (c),e		;0c9c	ed 59 	. Y
	ret			;0c9e	c9 	.
sub_0c9fh:
	ld a,l			;0c9f	7d 	}
	or 0c0h		;0ca0	f6 c0 	. .
	ld l,a			;0ca2	6f 	o
	ld de,0020h		;0ca3	11 20 00 	.   .
	ld b,h			;0ca6	44 	D
	ld a,050h		;0ca7	3e 50 	> P
l0ca9h:
	ld c,0eeh		;0ca9	0e ee 	. .
	cp b			;0cab	b8 	.
	ret z			;0cac	c8 	.
	out (c),l		;0cad	ed 69 	. i
	ld c,DDSPY
	out (c),d		;0cb1	ed 51 	. Q
	ld c,SDSPY
	set 5,l		;0cb5	cb ed 	. .
	out (c),l		;0cb7	ed 69 	. i
	ld c,DDSPY
	out (c),e		;0cbb	ed 59 	. Y
	res 5,l		;0cbd	cb ad 	. .
	inc b			;0cbf	04 	.
	jr l0ca9h		;0cc0	18 e7 	. .
	ld a,d			;0cc2	7a 	z
	or 0c0h		;0cc3	f6 c0 	. .
	ld d,a			;0cc5	57 	W
	ld a,e			;0cc6	7b 	{
	or 0c0h		;0cc7	f6 c0 	. .
	ld e,a			;0cc9	5f 	_
	ld b,050h		;0cca	06 50 	. P
	push hl			;0ccc	e5 	.
	push bc			;0ccd	c5 	.
	call sub_0cd7h		;0cce	cd d7 0c 	. . .
	pop bc			;0cd1	c1 	.
	pop hl			;0cd2	e1 	.
	set 5,d		;0cd3	cb ea 	. .
	set 5,e		;0cd5	cb eb 	. .
sub_0cd7h:
	ld c,SDSPY
	out (c),d		;0cd9	ed 51 	. Q
	ld hl,DSPBUF		;0cdb	21 26 fd 	! & .
	push hl			;0cde	e5 	.
	push bc			;0cdf	c5 	.
	ld c,DDSPY
	inir		;0ce2	ed b2 	. .
	pop bc			;0ce4	c1 	.
	pop hl			;0ce5	e1 	.
	out (c),e		;0ce6	ed 59 	. Y
	ld c,DDSPY
	otir
	ret

;|------------------------------|
;| INTERRUPT SERVICE ROUTINES	|
;|------------------------------|
;
;	50/60 HZ INTERRUPT (0xf4)
INTF4:
	push hl
	push de
	push bc
	push af
	in a,(DPIOB)
	bit 6,a			; Key pressed
	jr z,_floppytimer	; Nope
	ld hl,CLICK
	push ix
	CALLIX SOUND	; 'click!'
	pop ix
	ld b,009h		; Perform 9 reads (first one is the start bit)
_loop:
	in a,(DPIOB)	; Read bit 6
	rlca			; Move to bit 7
	rlca			; Move to carry
	ccf				; Invert
	rr l			; Stuff it back into l
	ld a,07fh		; Clock low
	out (DPIOB),a	;
	ld a,0ffh		; Clock high
	out (DPIOB),a	;
	djnz _loop		; Loop over data bits
	bit 6,l			; If bit 6 is set...
	jr z,_skip		;
	res 7,l			; ...clear bit 7
_skip:
	ld h,0			; hl = l
	ld de,KEYTBL
	add hl,de
	ld a,(hl)		; key at KEYTBL+data
	cp 0xfe			; Shift lock
	ld a,(SHLOCK)
	jr nz,_cont		; No shift, shlock
	cpl				; Invert shif
	ld (SHLOCK),a
	jr _floppytimer
_cont:
	or a			; Shift lock?
	ld a,(hl)		; Get key again
	jr z,_gotkey	; No shift lock, we're done
	cp 061h			; <'a'
	jr c,_gotkey	; Less than 'a', we're done
	cp 07bh			; >'z'
	jp p,_gotkey	; More than 'z', we're done
	res 5,a			; Make uppercase
_gotkey:
	ld (KEYCOD),a	; We got a key
	ld a,0xff
	ld (KEYFLG),a	; And there is a key in the buffer

		;	FLOPPY MOTOR SHUT-DOWN
_floppytimer:
	ld hl,(MTRCNT)		; Get motor timer
	ld a,h
	or l
	jr nz,_end		; If <> 0 then decrement & exit

	; PIO Port A: bit mode & every line as output
	ld a,0cfh
	out (SPIOA),a
	xor a
	ld (HOMES),a	; Clear homes
	out (SPIOA),a

	ld a,(LSTATE)	; Always 0xC0 = motor on + graphics enable
	res 0,a		; Clear motor on
	out (DPIOA),a

	; DPIOB: Latch load & operation stobe
	ld bc,0fbh*256+DPIOB
	out (c),b
	res 5,b
	out (c),b
	set 5,b
	out (c),b
	inc hl ; Keep at zero. Will motor shutdown be spammed?
_end:
	dec hl
	ld (MTRCNT),hl

; Common interrupt exit
IEND:
	pop af			;0d73	f1 	.
	pop bc			;0d74	c1 	.
	pop de			;0d75	d1 	.
	pop hl			;0d76	e1 	.
	ei			;0d77	fb 	.
	reti		;0d78	ed 4d 	. M

;	FLOPPY INTERRUPT ROUTINE
SRVFPY:
	push hl			;0d7a	e5 	.
	push de			;0d7b	d5 	.
	push bc			;0d7c	c5 	.
	push af			;0d7d	f5 	.
	ld a,0ffh		;0d7e	3e ff 	> .
	ld (FPYFLG),a		;0d80	32 1e fd 	2 . .
	in a,(SFLPY)		;0d83	db fc 	. .
	and 010h		;0d85	e6 10 	. .
	jr z,l0d99h		;0d87	28 10 	( .
	ld hl,ST0		;0d89	21 15 fd 	! . .
	ld b,007h		;0d8c	06 07 	. .
l0d8eh:
	push bc			;0d8e	c5 	.
	call READ_FDC_REG		;0d8f	cd a5 0a 	. . .
	ld (hl),a			;0d92	77 	w
	inc hl			;0d93	23 	#
	pop bc			;0d94	c1 	.
	djnz l0d8eh		;0d95	10 f7 	. .
	jr IEND		;0d97	18 da 	. .
l0d99h:
	ld c,008h		;0d99	0e 08 	. .
	call SEND_FDC_CMD
	call READ_FDC_REG
	jr nz,l0dabh		;0da1	20 08 	  .
	bit 7,a		;0da3	cb 7f 	.
	jr z,l0dabh		;0da5	28 04 	( .
	bit 6,a		;0da7	cb 77 	. w
	jr z,IEND		;0da9	28 c8 	( .
l0dabh:
	ld (ST0),a		;0dab	32 15 fd 	2 . .
	call READ_FDC_REG	;0dae	cd a5 0a 	. . .
	ld (CYL),a		;0db1	32 18 fd 	2 . .
	jr IEND		;0db4	18 bd 	. .

IOINITDATA:
	; CTRC (TMS9927 = CRT5027) init

	; Start timing chain
	db SDSPY,  1, 0aeh
	db DDSPY,  1, 000h

	; Reset
	db SDSPY,  1, 0aah
	db DDSPY,  1, 000h

	; Horizontal character count: 97
	db SDSPY,  1, 0a0h
	db DDSPY,  1, 061h

	; Non-interlaced
	; Horizontal sync width: 10 characters
	; Horizontal sync delay: 1
	db SDSPY,  1, 0a1h
	db DDSPY,  1, 061h

	; Scans/data row: 9
	; Characters/data row: 80
	db SDSPY,  1, 0a2h
	db DDSPY,  1, 04dh

	; Sync & blank delay: 1 characters
	; Cursor delay: 0 characters
	; Data rows/frame: 35
	db SDSPY,  1, 0a3h
	db DDSPY,  1, 097h

	; Scan lines/frame: 3
	db SDSPY,  1, 0a4h
	db DDSPY,  1, 003h

	; Vertical data start: 12
	db SDSPY,  1, 0a5h
	db DDSPY,  1, 00ch

	; Last displayed data row: 23
	db SDSPY,  1, 0a6h
	db DDSPY,  1, 017h

	; Start timing chain
	db SDSPY,  1, 0aeh
	db DDSPY,  1, 000h

	db DPIOB,  1, 0ffh
	db SPIOA,  3, 04fh, 0cfh, 000h
	db SPIOB,  3, 04fh, 0cfh, 040h
	db DPIOA,  1, 0c0h
	db DPIOB,  3, 0fbh, 03bh, 0fbh

	db DMATMP, 1, 000h
	db DMACSR, 1, 040h
	db DMAWMR, 4, 048h, 049h, 04ah, 04bh

	db DSPBCA, 2, 000h, 00
	db DSPBWR, 2, 000h, 00

	db BAUDC,  4, 003h, 0f0h, 057h, 02h
	db BAUDP,  2, 057h, 02

	db DSPINT, 3, 003h, 0d5h, 001h
	db FPYINT, 3, 003h, 0d5h, 001h

	db SCOMM,  11, 000h, 00, 0d8h, 04h, 044h, 001h, 060h, 003h, 0c1h, 05h, 0eah
	db SPRTR,  11, 000h, 00, 0d8h, 04h, 044h, 001h, 060h, 003h, 0c1h, 05h, 0eah

	;   End of table
	db 0ffh

;SHIFT
KEYTBL: db 0x08, 0x09, 0x0A, 0x00, 0x00, 0x0D, 0x00, 0xFE	;BS TAB LF N/A N/A CR N/A LOCK
	db 0x20, 0x00, 0x00, 0x1B, 0x81, 0x82, 0x83, 0x84	;SP NA NA ESC LFT RT UP DN
	db 0x5e, 0x21, 0x40, 0x23, 0x24, 0x25, 0x26, 0x2a	;'^!@#$%&*
	db 0x28, 0x29, 0x22, 0x3a, 0x3c, 0x2b, 0x3e, 0x3f	;()":<+>?
	db 0x7e, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47	;~ABCDEFG
	db 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f	;HIJKLMNO
	db 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57	;PQRSTUVW
	db 0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x5f, 0x07	;XYZ{|}_   bell

;CTRL
; Ctrl+LF = 0x86
	db 0x08, 0x09, 0x86, 0x00, 0x00, 0x0D, 0x00, 0xFE	;BS TAB LF N/A N/A CR N/A LOCK
	db 0x20, 0x00, 0x00, 0x1B, 0x81, 0x82, 0x83, 0x84	;SP NA NA ESC LFT RT UP DN
	db 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37	;01234567
	db 0x38, 0x39, 0x3a, 0x3b, 0x2c, 0x3d, 0x2e, 0x2f	;89:;,=./
	db 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07	;CTRL-@ABCDEFG
	db 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f	;CTRL-HIJKLMNO
	db 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17	;CTRL-PQRSTUVW
	db 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f	;CTRL-XYZ[\]_

;NO SHIFT OR CTRL
	db 0x08, 0x09, 0x0A, 0x00, 0x00, 0x0D, 0x00, 0xFE	;BS TAB LF N/A N/A CR N/A LOCK
	db 0x20, 0x00, 0x00, 0x1B, 0x81, 0x82, 0x83, 0x84	;SP NA NA ESC LFT RT UP DN

	db 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37	;01234567
	db 0x38, 0x39, 0x27, 0x3b, 0x2c, 0x3d, 0x2e, 0x2f	;89';,=./
	db 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67	;`abcdefg
	db 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f	;hijklmno
	db 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77	;pqrstuvw
	db 0x78, 0x79, 0x7a, 0x5b, 0x5c, 0x5d, 0x2d, 0x7F	;xyz[\]-    bell

;GENERAL FLOPPY HANDLER
;(A,B,C,D,E,H,L)
;READS A TABLE TO DO DISK OPERATIONS
;  FIRST BYTE
;	0-3 = # OF BYTES TO SEND TO 765
;	4 = 1 IF DMA TO BE ACTIVATED
;	5 = 1 IF INTERRUPT IS TO BE PROCESSED
;	6 = 1 IF WRITE TO DISK, ELSE READ
;	7 = 1 IF ST3 IS TO BE READ
;  IF DMA IS NEEDED, THE NEXT TO BYTES ARE
;    THE START LOCATION IN MEMORY
;  NEXT BYTES ARE TRANSFERRED TO THE 765
; IF ERROR, RETURN ADDRESS IN X REG.
;  (USUALLY CONTAINS ADDRESS OF ERROR ROUTINE)
;

FPYTBL:
	db 0x39		; 9 bytes | DMA | INT
	dw 0xfe00
	dw 0x1ff
	db DBL*40H+6	; Read data, DBL sets density flag (FM or MFM)
	db 0		; Drive 0 / Head 0
	db 0		; Cylinder 0
	db 0		; Head 0
	db 1		; Sector 0
	db DBL+1	; Bytes per sector
	db SECS+9	; EOT = Sector number per cylinder
	db 37-(SECS*22)	; GPL = Gap between sectors
	db 0xff		; DTL = Sector data length

RECALIBRATE: ;0f18, unused?
	db 0x22		; 2 bytes | DMA
	dw 7
	dw 0x0F23
	db 0		; Invalid?
	db 5

SPECIFY:
	db 3
	db 3		; 765: Specify
	db SRT*16+HUT	; Step Rate Time + Head Unload Time
	db HLT*2+ND	; Head Load Time + Non DMA Mode

FORMAT:
	db 0x76		; 6 bytes | DMA | INT | Write
	dw FILBUF	; DMA Address
	dw 4*(9+SECS)-1
	db 40H*DBL+0DH	; Format, DBL sets density flag (FM or MFM)
	db 0		; Drive 0 / Head 0
	db DBL+1	; Bytes per sector
	db SECS+9	; Sectors per track (10)
	db 87-(SECS*57)	; Gap between sectors
	db 0xE5		; Filler data pattern

;;
;FMTTBL:	.BYTE	76H
;	.WORD	FILBUF
;	.WORD	4*(9+SECS)-1
;	.BYTE	40H*DBL+0DH
;	.BYTE	0	;HD & DRIVE
;	.BYTE	DBL+1	;SECTOR SIZE
;	.BYTE	SECS+9	;SECTORS/CYL
;	.BYTE	87-(SECS*57) ;GAP 3
;	.BYTE	0E5H	;FILL DATA


; Baud rate table
	xor (hl)		;0f2e	ae 	.
	add a,b			;0f2f	80 	.
	ld b,b			;0f30	40 	@
	jr nz,$+18		;0f31	20 10 	  .
	ex af,af'			;0f33	08 	.
	inc b			;0f34	04 	.
	ld (bc),a			;0f35	02 	.
	ld bc,0040h		;0f36	01 40 00 	. @ .
	add a,b			;0f39	80 	.
	ret nz			;0f3a	c0 	.
	nop			;0f3b	00 	.
	nop			;0f3c	00 	.
	nop			;0f3d	00 	.
	nop			;0f3e	00 	.
l0f3fh:
	adc a,c			;0f3f	89 	.
	xor l			;0f40	ad 	.
	ei			;0f41	fb 	.
	call pe,0db89h		;0f42	ec 89 db 	. . .
	jp pe,08ccfh		;0f45	ea cf 8c 	. . .
	xor e			;0f48	ab 	.
	exx			;0f49	d9 	.
	rst 28h			;0f4a	ef 	.
	exx			;0f4b	d9 	.
	xor e			;0f4c	ab 	.
	ret z			;0f4d	c8 	.
	rst 28h			;0f4e	ef 	.
MEMMAPFTO8:
		;	Memory map
	db 0xfe, 0xdc, 0xba, 0x98
MEMMAP8TOF:
		;	Memory map
	db 0x89, 0xab, 0xcd, 0xef

; United test sequence
UNITSEQ:
	db "HQTMR1R2R3RZ1Z", 0
NODISK_MSG:
	db "\r\nNO DISK OR DISK NOT READABL", "E"|0x80
NOSYS_MSG:
	db "\r\nNO SYSTEM ON DIS", "K"|0x80
TERMINAL_MSG:
	db "\r\nNOW IN TERMINAL MODE\r\n", "\n"|0x80

; Blank
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0


; Sound table

CLICK:
	db 0x00, 0x01, 0x80, 0x1f, 0x1f, 0x1f, 0x38, 0x00
	db 0x00, 0x80, 0x00, 0x40, 0x01, 0x00

BEEP:
	db 0x02, 0x12, 0x00, 0x1f, 0x1f, 0x1f, 0x38, 0x00
	db 0x00, 0x20, 0x00, 0x40, 0x00, 0x10

	db 0, 0, 0, 0

	dw RESET
	dw RESET
	dw INTF4				; Interrupt F4
	dw SRVFPY				; Interrupt F6
	dw RESET
	dw RESET
	dw RESET
	dw RESET

;
;|----------------------|
;|	 RAM AREA	|
;|----------------------|
;
;
;	DISK VARIABLES
;
;	DISK COMMAND TABLES (LEAVE AS IS UP TO ERRCNT)
;
;	--- READ/WRITE/FORMAT ---
;
TYPE: equ 0xfd00	;COMMAND INFO
			;  NNNNNNNN
			;  ||||||||
			;  ||||BYTES TO 765
			;  |||0=NO, 1=YES FOR DMA
			;  ||0=NO, 1=YES FOR INT
			;  |0=RD, 1=WR TO DISK
			;  0=NO, 1=YES TO READ ST3
CMMD: equ 0xfd05	; Cursor position row, column
RDRV: equ 0xfd06	; UNIT #

;DMAADR:	.BLKW	1	;ADDRESS TO START DATA XFER
;BCNT:	.BLKW	1	;BYTE COUNT-1
;CMMD:	.BLKB	1	;COMMAND TO 765
;RDRV:	.BLKB	1	;UNIT #
RDCYL: equ 0xfd07	; CYLINDER #
RHD: equ 0xfd08		;HEAD #
RCRD: equ 0xfd09	;RECORD #
;NSENT:	.BLKB	1	;BYTES/SECTOR
;EOT:	.BLKB	1	;SECTORS/TRACK
;GPL:	.BLKB	1	;GAP 2 LENGTH
;DTL:	.BLKB	1	;DATA LENGTH

;
;	--- RECALIBRATE	---
;
RCLTBL: equ 0xfd0e	; RECAL TYPE, RECAL COMMAND
HDRV: equ 0xfd10	; RECAL DRIVE

;
;	--- SEEK ---
;
SEKTBL: equ 0xfd11	; SEEK TYPE, SEEK COMMAND
SDRV: equ 0xfd13	; SEEK DRIVE
NCN: equ 0xfd14		;NEW CYLINDER NUMBER

;
;	READ BACK STATUS - MUST STAY IN ORDER
;
ST0:  equ 0xfd15	; STO STATUS
ST1:  equ 0xfd16	; ST1 STATUS
ST2:  equ 0xfd17	; ST2 STATUS
CYL:  equ 0xfd18	; CYLINDER STATUS
HD:   equ 0xfd19	; HEAD STATUS
REC:  equ 0xfd1a	; RECORD STATUS
NUMB: equ 0xfd1b	; NUMBER OF BYTES STATUS
ST3:  equ 0xfd1c	; DRIVE STATUS

;
;	OTHER FLOPPY VARIABLES
;
ERRCNT: equ 0xfd1d	; Disk error counter
FPYFLG: equ 0xfd1e	; Floppy flag (FF = INTERRUPT TAKEN)
HOMES:  equ 0xfd1f	; Home flags (not sure what it means)
MTRCNT: equ 0xfd20	; Floppy motor timer

; HIGH MEMORY VARIABLES
CURSOR: equ 0xfd22	; Cursor position row, column
CURSORROW: equ 0xfd22	; Cursor row
CURSORCOL: equ 0xfd23	; Cursor column
CURATT:	equ 0xfd24	;CURRENT ATTRIBUTE CODE
CURCHR:	equ 0xfd25	;CURRENT CHARACTER SET
DSPBUF:	equ 0xfd26	;BUFFER FOR MOVING LINES
DSPCYC: equ 0xfd76	; DISPLAY CYCLE COUNTER
			;	0 = NORMAL CHAR
			;	1 = ESC PENDING
			;	2 = LINE # PNDG.
			;	3 = CHAR # PNDG.
			;	4 = ATTRIBUTE PNDG.
			;	5 = CHARACTER SET PNDG.
			;	6 = LEAD-IN PENDING
LINOFS:	equ 0xfd77	;OFFSET DUE TO LINE FEEDS

;
;	KEYBOARD VARIABLES
;
KEYCOD: equ 0xfd78		; Key code
KEYFLG: equ 0xfd79		; Key flag (FF = KEY WAITING)
SHLOCK: equ 0xfd7a		; Shift lock

;
;	MISC. VARIABLES
;
; FLAG86:	equ 0xfd00	; 8086 FLAG; 0 = O.K.
BOOTRY:	equ 0xfd7b	; BOOT TRY COUNTER
BOTSTK:	equ 0xfd7c	; BOOT STACK STORAGE
CMDPTR: equ 0xfd7e	; Command pointer (for macros)
FLGLOP: equ 0xfd80	; LOOP PENDING FLAG
FLGCMD:	equ 0xfd81	; MACRO FLAG
LSTATE: equ 0xfd82	; "L0-L7 State" (for floppy)
CMDSTT:	equ 0xfd83	;START OF MACPTR
OUTSEL:	equ 0xfd85	;OUTPUT SELECT STATUS
SAVDE:  equ 0xfd86	; Loop DE save
SAVHL:	equ 0xfd88	;LOOP HL SAVE
SAVCMD: equ 0xfd8a	; Saved command (not used)
SAVDE2:	equ 0xfd8b
SAVHL2:	equ 0xfd8d
SVCMD2:	equ 0xfd8f
CMDBUF:	equ 0xfd90	;U COMMAND BUFFER
