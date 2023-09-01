; z80dasm 1.1.6
; command line: z80dasm -l -t -g 0 -a Otrona_Attaché_U252_Rev_D.BIN

	org	00000h

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
DFLPY:	equ 0fdh	;FLOPPY DATA
DDSPY:	equ 0feh	;DISPLAY DATA
DMAP:	equ 0ffh	;RAM VIRTUAL MAP DATA

;   The stack
STACK_BASE: equ $fe00





RESET:
	jp BOOT         ; Called at boot
	jp l062fh		;0003	c3 2f 06 	. / .
	jp l0865h		;0006	c3 65 08 	. e .
	jp l0abah		;0009	c3 ba 0a 	. . .
	jp l090eh		;000c	c3 0e 09 	. . .

WELCOME_MSG:
	db $1b, 'J'    ; ESC-J , probably erase to end of screen
    db 'OTRONA ATTACHE'
    db $d, $8a

        ;   BOOT, we init the hardware
BOOT:
	di			       
	ld hl,IOINITDATA	; I/O registers configuration table
LOOP_IOREG:
	ld a,(hl)			; Load I/O address to output
	ld c,a			
	inc a			   
	jr z,REGTEST		; If address is 0xff, we're done
	inc hl			    
	ld b,(hl)			; Read number of bytes to output
	inc hl			    ;
LOOP_IODATA:
	ld a,(hl)			; Byte to output to I/O
	out (c),a		    ; Output to I/O register
	inc hl			    
	djnz LOOP_IODATA	; Still bytes for this register?
	jr LOOP_IOREG		; Go to next register

        ;   Now we test if the CPU works well (this sounds quite strange)

    ;   Test the registers by passing all 1s and all0s across all of them
REGTEST:	ld a,%11111111
	scf			        ; carry set
	ld i,a		        ; pass value throught i
	ld a,i		
PASSVALUE:              ; passes a value (all 1s, then all 0s, through all the registers)
	ld b,a			    ; pass value through b,c,d,e,l,h
	ld c,b			
	ld d,c			
	ld e,d			
	ld l,e			
	ld h,l			
	ex af,af'		    ; value goes to other register bank
	ld a,h			    ; a contains the passed value
	exx                 ; now same exercice with the other bank
	ld b,a			
	ld c,b			
	ld d,c			
	ld e,d			
	ld l,e			
	ld h,l			
	ex af,af'		    ; And get the other a to fill, and the carry
	ld a,h			    ; and fill it
	jr nc,OTHER_TEST		; second loop? (c was 1 first time)
	cpl			        ; swap 0 and 1
	and a			    ; test
	jr z,PASSVALUE		; should be zero, and a is now %00000000
                        ; carry is not set, so we'll go to OTHER_TEST if everything is good

BOOT_FAILED:
	halt			    ; We failed to boot

OTHER_TEST:
	jr nz,BOOT_FAILED	; Second time, a should be zero
	or a			    ; Test that "or 0" is 0.
	jr nz,BOOT_FAILED

    ;   Test the ix, iy, sp registers and dual byte additions/flags
                        ; note: all registers are 0
	ld ix,$ffff		    ;
	ld sp,ix		    ;
	add hl,sp			; hl was 0, so should now be $ffff
	ex de,hl			; de=$ffff, hl=0

	ld iy,$ffff		    ; same dance with iy
	ld sp,iy		    ;
	add hl,sp			; $ffff
	add hl,de			; $fffe
	jr nc,BOOT_FAILED	; We clearly had a carry, right?

	inc hl			    ; now $ffff
	ld ix,0
	ld sp,ix
	add hl,sp           ; no change
	ex de,hl			; de=$ffff, hl=$ffff

	ld iy,0
    ld sp,iy
	add hl,sp			; $ffff
	add hl,de			; $fffe
	inc hl			    ; $ffff
	ld a,h			    ; $ff
	xor l			    ; $ff^$ff = $00
	jr nz,BOOT_FAILED	; if we did not land on zero, something wrong

    ;   Test basic flags behavior
	ld a,$40
	add a,a
	jr z,BOOT_FAILED	; $80 is not 0
	jr c,BOOT_FAILED	; $40+$40 does not create carry
	jp p,BOOT_FAILED	; $80 is -128, so if positive, failed
	jp po,BOOT_FAILED	; not overflow is tested (parity and overflow shares a flag)
	add a,a			    ; $80+$80 = $00
	jr nz,BOOT_FAILED	; Not zero? Fail
	jr nc,BOOT_FAILED	; If no carry, fail
	jp m,BOOT_FAILED	; If negative, fail
	or 1		        ; aka or a
	jp pe,BOOT_FAILED	; parity is even? Fail

    ;   Test decimal adjustment instruction
	ld a,9
	add a,1
	daa                 ; decimal 10 adjusted to decimal
	cp 010h		        ; is hex 10
	jr nz,BOOT_FAILED	; or Fail
	sbc a,001h		    ; Carry was set to zero by daa, a is $f
	daa			        ; hex f adjusted is 9
	cp 9
	jr nz,BOOT_FAILED	; not 9? Fail

    ;   (unsure)
	ld sp,STACK_BASE	; Initial stack
	ld hl,l0f53h		;00ae	21 53 0f 	! S .
	ld ix,CONT		;00b1	dd 21 b8 00 	. ! . .
	jp l0711h		;00b5	c3 11 07 	. . .

CONT:
	xor a			;00b8	af 	.
	ld i,a		;00b9	ed 47 	. G
	ld hl,0fd00h		;00bb	21 00 fd 	! . .
l00beh:
	ld c,0ffh		;00be	0e ff 	. .
l00c0h:
	ld (hl),c			;00c0	71 	q
	ld a,c			;00c1	79 	y
	xor (hl)			;00c2	ae 	.
	jp nz,l03d9h		;00c3	c2 d9 03 	. . .
	sla c		;00c6	cb 21 	. !
	jr c,l00c0h		;00c8	38 f6 	8 .
	inc l			;00ca	2c 	,
	jr nz,l00beh		;00cb	20 f1 	  .
	cpl			;00cd	2f 	/
	ld (0fd7ah),a		;00ce	32 7a fd 	2 z .
	ld a,0c0h		;00d1	3e c0 	> .
	ld (0fd82h),a		;00d3	32 82 fd 	2 . .
	ld d,002h		;00d6	16 02 	. .
	call sub_06cbh		;00d8	cd cb 06 	. . .
	ld a,c			;00db	79 	y
	cp 009h		;00dc	fe 09 	. .
	jr nz,l00f0h		;00de	20 10 	  .
	ld a,0a4h		;00e0	3e a4 	> .
	out (SDSPY),a		;00e2	d3 ee 	. .
	ld a,01dh		;00e4	3e 1d 	> .
	out (DDSPY),a		;00e6	d3 fe 	. .
	ld a,0a5h		;00e8	3e a5 	> .
	out (SDSPY),a		;00ea	d3 ee 	. .
	ld a,025h		;00ec	3e 25 	> %
	out (DDSPY),a		;00ee	d3 fe 	. .
l00f0h:
	ld a,00fh		;00f0	3e 0f 	> .
	ld i,a		;00f2	ed 47 	. G
	im 2		;00f4	ed 5e 	. ^
	ei			;00f6	fb 	.
	ld hl,WELCOME_MSG		;00f7	21 0f 00 	! . .
	ld iy,l0101h		;00fa	fd 21 01 01 	. ! . .
	jp DISP_HL		;00fe	c3 2c 07 	. , .
l0101h:
	jp l0a1bh		;0101	c3 1b 0a 	. . .
l0104h:
	ld c,00dh		;0104	0e 0d 	. .
	ld ix,l010dh		;0106	dd 21 0d 01 	. ! . .
	jp l07e3h		;010a	c3 e3 07 	. . .
l010dh:
	ld c,00ah		;010d	0e 0a 	. .
	ld ix,l0116h		;010f	dd 21 16 01 	. ! . .
	jp l07e3h		;0113	c3 e3 07 	. . .
l0116h:
	ld c,040h		;0116	0e 40 	. @
	ld ix,l011fh		;0118	dd 21 1f 01 	. ! . .
	jp l07e3h		;011c	c3 e3 07 	. . .
l011fh:
	ld de,0		;011f	11 00 00 	. . .
	ld (0fd86h),de		;0122	ed 53 86 fd 	. S . .
l0126h:
	ld h,000h		;0126	26 00 	& .
	ld l,h			;0128	6c 	l
	ld b,h			;0129	44 	D
l012ah:
	ld a,i		;012a	ed 57 	. W
l012ch:
	or a			;012c	b7 	.
	jr z,l0173h		;012d	28 44 	( D
	call sub_061ch		;012f	cd 1c 06 	. . .
	ld a,(0fd81h)		;0132	3a 81 fd 	: . .
	or a			;0135	b7 	.
	jr z,l0148h		;0136	28 10 	( .
	push hl			;0138	e5 	.
	ld hl,(0fd7eh)		;0139	2a 7e fd 	* ~ .
	ld a,(hl)			;013c	7e 	~
	inc hl			;013d	23 	#
	ld (0fd7eh),hl		;013e	22 7e fd 	" ~ .
	pop hl			;0141	e1 	.
	or a			;0142	b7 	.
	jr nz,l017eh		;0143	20 39 	  9
	ld (0fd81h),a		;0145	32 81 fd 	2 . .
l0148h:
	ld a,(0fd80h)		;0148	3a 80 fd 	: . .
	cp 080h		;014b	fe 80 	. .
	jr nz,l0161h		;014d	20 12 	  .
	ld hl,0fd83h		;014f	21 83 fd 	! . .
	ld (0fd7eh),hl		;0152	22 7e fd 	" ~ .
	ld hl,(0fd8dh)		;0155	2a 8d fd 	* . .
	ld de,(0fd8bh)		;0158	ed 5b 8b fd 	. [ . .
	ld a,(0fd8fh)		;015c	3a 8f fd 	: . .
	jr l017eh		;015f	18 1d 	. .
l0161h:
	ld iy,l0168h		;0161	fd 21 68 01 	. ! h .
	jp l081ah		;0165	c3 1a 08 	. . .
l0168h:
	jr z,l0173h		;0168	28 09 	( .
	xor a			;016a	af 	.
	ld (0fd79h),a		;016b	32 79 fd 	2 y .
	ld a,(0fd78h)		;016e	3a 78 fd 	: x .
	jr l017eh		;0171	18 0b 	. .
l0173h:
	ld iy,l017ah		;0173	fd 21 7a 01 	. ! z .
	jp l0790h		;0177	c3 90 07 	. . .
l017ah:
	jr z,l0161h		;017a	28 e5 	( .
	in a,(DCOMM)		;017c	db f0 	. .
l017eh:
	bit 6,a		;017e	cb 77 	. w
	jr z,l0184h		;0180	28 02 	( .
	res 5,a		;0182	cb af 	. .
l0184h:
	ld c,a			;0184	4f 	O
	ld ix,l018ch		;0185	dd 21 8c 01 	. ! . .
	jp l07e3h		;0189	c3 e3 07 	. . .
l018ch:
	ld (0fd8ah),a		;018c	32 8a fd 	2 . .
	cp 008h		;018f	fe 08 	. .
	jr nz,l019ch		;0191	20 09 	  .
	ex de,hl			;0193	eb 	.
	xor a			;0194	af 	.
	or b			;0195	b0 	.
	jr z,l0199h		;0196	28 01 	( .
	ld (hl),e			;0198	73 	s
l0199h:
	dec hl			;0199	2b 	+
	jr l01bah		;019a	18 1e 	. .
l019ch:
	cp 00dh		;019c	fe 0d 	. .
	jr nz,l01aah		;019e	20 0a 	  .
	xor a			;01a0	af 	.
	or b			;01a1	b0 	.
	jp z,l062fh		;01a2	ca 2f 06 	. / .
	ex de,hl			;01a5	eb 	.
	ld (hl),e			;01a6	73 	s
	jp l062fh		;01a7	c3 2f 06 	. / .
l01aah:
	cp 086h		;01aa	fe 86 	. .
	jp z,l0a6eh		;01ac	ca 6e 0a 	. n .
	cp 00ah		;01af	fe 0a 	. .
	jr nz,l01dbh		;01b1	20 28 	  (
	ex de,hl			;01b3	eb 	.
	xor a			;01b4	af 	.
	or b			;01b5	b0 	.
	jr z,l01b9h		;01b6	28 01 	( .
	ld (hl),e			;01b8	73 	s
l01b9h:
	inc hl			;01b9	23 	#
l01bah:
	ld c,00dh		;01ba	0e 0d 	. .
	ld ix,l01c3h		;01bc	dd 21 c3 01 	. ! . .
	jp l07e3h		;01c0	c3 e3 07 	. . .
l01c3h:
	ld iy,l01cah		;01c3	fd 21 ca 01 	. ! . .
	jp l0698h		;01c7	c3 98 06 	. . .
l01cah:
	ld a,h			;01ca	7c 	|
	ld h,l			;01cb	65 	e
	ld l,a			;01cc	6f 	o
	ld iy,l01d4h		;01cd	fd 21 d4 01 	. ! . .
	jp l0698h		;01d1	c3 98 06 	. . .
l01d4h:
	ld a,l			;01d4	7d 	}
	ld l,h			;01d5	6c 	l
	ld h,a			;01d6	67 	g
	ld a,02fh		;01d7	3e 2f 	> /
	jr l0184h		;01d9	18 a9 	. .
l01dbh:
	cp 02fh		;01db	fe 2f 	. /
	jr nz,l01fah		;01dd	20 1b 	  .
	ld b,000h		;01df	06 00 	. .
	ld d,h			;01e1	54 	T
	ld e,l			;01e2	5d 	]
	ld h,(hl)			;01e3	66 	f
	ld iy,l01ebh		;01e4	fd 21 eb 01 	. ! . .
	jp l0698h		;01e8	c3 98 06 	. . .
l01ebh:
	ld c,020h		;01eb	0e 20 	.  
	ld ix,l01f4h		;01ed	dd 21 f4 01 	. ! . .
	jp l07e3h		;01f1	c3 e3 07 	. . .
l01f4h:
	ld h,000h		;01f4	26 00 	& .
	ld l,h			;01f6	6c 	l
	jp l012ah		;01f7	c3 2a 01 	. * .
l01fah:
	cp 020h		;01fa	fe 20 	.  
	jr nz,l0205h		;01fc	20 07 	  .
	ld (0fd86h),hl		;01fe	22 86 fd 	" . .
	ex de,hl			;0201	eb 	.
	jp l0126h		;0202	c3 26 01 	. & .
l0205h:
	cp 047h		;0205	fe 47 	. G
	jr nz,l0241h		;0207	20 38 	  8
	ld c,01eh		;0209	0e 1e 	. .
	call l0abah		;020b	cd ba 0a 	. . .
	ld de,0087fh		;020e	11 7f 08 	.  .
	ld c,02bh		;0211	0e 2b 	. +
l0213h:
	call l0abah		;0213	cd ba 0a 	. . .
	dec e			;0216	1d 	.
	jr nz,l0213h		;0217	20 fa 	  .
	dec d			;0219	15 	.
	jr nz,l0213h		;021a	20 f7 	  .
	call sub_065ah		;021c	cd 5a 06 	. Z .
	bit 7,d		;021f	cb 7a 	. z
	jr z,l0236h		;0221	28 13 	( .
	ld d,0cfh		;0223	16 cf 	. .
	call sub_06cbh		;0225	cd cb 06 	. . .
	ld a,088h		;0228	3e 88 	> .
	out (DPIOB),a		;022a	d3 fa 	. .
l022ch:
	in a,(DPIOB)		;022c	db fa 	. .
	bit 6,a		;022e	cb 77 	. w
	jr z,l022ch		;0230	28 fa 	( .
	ei			;0232	fb 	.
	jp l0620h		;0233	c3 20 06 	.   .
l0236h:
	ld d,04fh		;0236	16 4f 	. O
	call sub_06bbh		;0238	cd bb 06 	. . .
	ei			;023b	fb 	.
	call sub_061ch		;023c	cd 1c 06 	. . .
	jr l0236h		;023f	18 f5 	. .
l0241h:
	cp 048h		;0241	fe 48 	. H
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
	cp 049h		;028d	fe 49 	. I
	jr nz,l029ah		;028f	20 09 	  .
	ld c,l			;0291	4d 	M
	in h,(c)		;0292	ed 60 	. `
	call sub_0696h		;0294	cd 96 06 	. . .
	jp l05f8h		;0297	c3 f8 05 	. . .
l029ah:
	cp 04ah		;029a	fe 4a 	. J
	jr nz,l02a3h		;029c	20 05 	  .
	ld de,l0104h		;029e	11 04 01 	. . .
	push de			;02a1	d5 	.
	jp (hl)			;02a2	e9 	.
l02a3h:
	cp 04bh		;02a3	fe 4b 	. K
	jr nz,l02bch		;02a5	20 15 	  .
l02a7h:
	call sub_076dh		;02a7	cd 6d 07 	. m .
	cp 05eh		;02aa	fe 5e 	. ^
	jp z,l062fh		;02ac	ca 2f 06 	. / .
	ld c,a			;02af	4f 	O
	call l0abah		;02b0	cd ba 0a 	. . .
	ld h,a			;02b3	67 	g
	call sub_0696h		;02b4	cd 96 06 	. . .
	call sub_07dfh		;02b7	cd df 07 	. . .
	jr l02a7h		;02ba	18 eb 	. .
l02bch:
	cp 'L'
	jr nz,l02c8h
	; Loop tests
	ld a,0xff
	ld (0fd80h),a		;02c2	32 80 fd 	2 . .
	jp l062fh		;02c5	c3 2f 06 	. / .
l02c8h:
	;	Suspicion: this is the memory mapping test 'M'
	cp 'M'
	jr nz,l031ah		;02ca	20 4e 	  N
	; Map Test
	di			;02cc	f3 	.
	ld hl,l0f4fh		;02cd	21 4f 0f 	! O .
	ld ix,l02d7h		;02d0	dd 21 d7 02 	. ! . .
	jp l0711h		;02d4	c3 11 07 	. . .
l02d7h:
	ld hl,0		;02d7	21 00 00 	! . .
	ld b,007h		;02da	06 07 	. .
l02dch:
	ld (hl),b			;02dc	70 	p
	ld a,h			;02dd	7c 	|
	add a,020h		;02de	c6 20 	.  
	ld h,a			;02e0	67 	g
	djnz l02dch		;02e1	10 f9 	. .
	ld hl,l0f53h		;02e3	21 53 0f 	! S .
	ld ix,l02edh		;02e6	dd 21 ed 02 	. ! . .
	jp l0711h		;02ea	c3 11 07 	. . .
l02edh:
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
	ld ix,l0308h		;0301	dd 21 08 03 	. ! . .
	jp l07e3h		;0305	c3 e3 07 	. . .
l0308h:
	ld h,d			;0308	62 	b
	ld iy,l0310h		;0309	fd 21 10 03 	. ! . .
	jp l0698h		;030d	c3 98 06 	. . .
l0310h:
	inc b			;0310	04 	.
	ld a,h			;0311	7c 	|
	add a,020h		;0312	c6 20 	.  
	ld h,a			;0314	67 	g
	jr nz,l02f8h		;0315	20 e1 	  .
	jp l05f8h		;0317	c3 f8 05 	. . .
l031ah:
	cp 04eh		;031a	fe 4e 	. N
	jr nz,l0321h		;031c	20 03 	  .
	jp l062fh		;031e	c3 2f 06 	. / .
l0321h:
	cp 04fh		;0321	fe 4f 	. O
	jr nz,l032bh		;0323	20 06 	  .
	ld c,h			;0325	4c 	L
	out (c),l		;0326	ed 69 	. i
	jp l05f8h		;0328	c3 f8 05 	. . .
l032bh:
	cp 050h		;032b	fe 50 	. P
	jr nz,l0335h		;032d	20 06 	  .
	call sub_09b5h		;032f	cd b5 09 	. . .
	jp l062fh		;0332	c3 2f 06 	. / .
l0335h:
	cp 051h		;0335	fe 51 	. Q
	jp nz,l036eh		;0337	c2 6e 03 	. n .
	ld e,00fh		;033a	1e 0f 	. .
l033ch:
	call sub_06cbh		;033c	cd cb 06 	. . .
	ld l,c			;033f	69 	i
	ld b,000h		;0340	06 00 	. .
l0342h:
	ld c,b			;0342	48 	H
	call sub_06cfh		;0343	cd cf 06 	. . .
	call sub_06cbh		;0346	cd cb 06 	. . .
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
	call sub_07dfh		;0365	cd df 07 	. . .
	call sub_0692h		;0368	cd 92 06 	. . .
l036bh:
	jp l05f8h		;036b	c3 f8 05 	. . .
l036eh:
	cp 052h		;036e	fe 52 	. R
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
	ld ix,l0398h		;0391	dd 21 98 03 	. ! . .
	jp l0711h		;0395	c3 11 07 	. . .
l0398h:
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
	jr nz,l03d9h		;03c3	20 14 	  .
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
l03d9h:
	ex af,af'			;03d9	08 	.
	ld c,020h		;03da	0e 20 	.  
	ld ix,l03e3h		;03dc	dd 21 e3 03 	. ! . .
	jp l07e3h		;03e0	c3 e3 07 	. . .
l03e3h:
	ld iy,003eah		;03e3	fd 21 ea 03 	. ! . .
	res 7,h		;03e7	cb bc 	. .
	jp l0698h		;03e9	c3 98 06 	. . .
	set 7,h		;03ec	cb fc 	. .
	ld a,h			;03ee	7c 	|
	ld h,l			;03ef	65 	e
	ld l,a			;03f0	6f 	o
	ld iy,l03f8h		;03f1	fd 21 f8 03 	. ! . .
	jp l0698h		;03f5	c3 98 06 	. . .
l03f8h:
	ld c,02dh		;03f8	0e 2d 	. -
	ld ix,l0401h		;03fa	dd 21 01 04 	. ! . .
	jp l07e3h		;03fe	c3 e3 07 	. . .
l0401h:
	ld a,h			;0401	7c 	|
	ex af,af'			;0402	08 	.
	ld h,a			;0403	67 	g
	ld iy,l040bh		;0404	fd 21 0b 04 	. ! . .
	jp l0698h		;0408	c3 98 06 	. . .
l040bh:
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
	ld hl,l0f53h		;0418	21 53 0f 	! S .
	ld ix,l0422h		;041b	dd 21 22 04 	. ! " .
	jp l0711h		;041f	c3 11 07 	. . .
l0422h:
	ld a,d			;0422	7a 	z
	and 003h		;0423	e6 03 	. .
	jr nz,l0432h		;0425	20 0b 	  .
	ld hl,09d00h		;0427	21 00 9d 	! . .
	ld de,0fd00h		;042a	11 00 fd 	. . .
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
	cp 053h		;043b	fe 53 	. S
	jr nz,l0469h		;043d	20 2a 	  *
	ld a,l			;043f	7d 	}
	ld (0fd85h),a		;0440	32 85 fd 	2 . .
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
	jp l062fh		;0453	c3 2f 06 	. / .
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
	cp 054h		;0469	fe 54 	. T
	jr nz,l0489h		;046b	20 1c 	  .
	call sub_065ah		;046d	cd 5a 06 	. Z .
	call sub_06bbh		;0470	cd bb 06 	. . .
	ld h,c			;0473	61 	a
	ld b,07dh		;0474	06 7d 	. }
	call sub_067ah		;0476	cd 7a 06 	. z .
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
	cp 055h		;0489	fe 55 	. U
	jr nz,l04cbh		;048b	20 3e 	  >
	ld a,(0fd80h)		;048d	3a 80 fd 	: . .
	cp 080h		;0490	fe 80 	. .
	jr nz,l0499h		;0492	20 05 	  .
	ld hl,(0fd83h)		;0494	2a 83 fd 	* . .
	jr l04c0h		;0497	18 27 	. '
l0499h:
	ld hl,0fd90h		;0499	21 90 fd 	! . .
	push hl			;049c	e5 	.
	ld (0fd7eh),hl		;049d	22 7e fd 	" ~ .
l04a0h:
	ld (hl),000h		;04a0	36 00 	6 .
	call sub_076dh		;04a2	cd 6d 07 	. m .
	ld c,a			;04a5	4f 	O
	call sub_07e1h		;04a6	cd e1 07 	. . .
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
	ld de,00f57h		;04b9	11 57 0f 	. W .
l04bch:
	ex de,hl			;04bc	eb 	.
	ld (0fd83h),hl		;04bd	22 83 fd 	" . .
l04c0h:
	ld (0fd7eh),hl		;04c0	22 7e fd 	" ~ .
	ld a,0ffh		;04c3	3e ff 	> .
	ld (0fd81h),a		;04c5	32 81 fd 	2 . .
	jp l05f8h		;04c8	c3 f8 05 	. . .
l04cbh:
	cp 056h		;04cb	fe 56 	. V
	jr nz,l04e1h		;04cd	20 12 	  .
	ld ix,l090eh		;04cf	dd 21 0e 09 	. ! . .
	call sub_0997h		;04d3	cd 97 09 	. . .
l04d6h:
	ld a,(0fd15h)		;04d6	3a 15 fd 	: . .
	and 0c0h		;04d9	e6 c0 	. .
	call nz,sub_0913h		;04db	c4 13 09 	. . .
	jp l05f8h		;04de	c3 f8 05 	. . .
l04e1h:
	cp 057h		;04e1	fe 57 	. W
	jr nz,l04eeh		;04e3	20 09 	  .
	ld ix,l090eh		;04e5	dd 21 0e 09 	. ! . .
	call sub_09a0h		;04e9	cd a0 09 	. . .
	jr l04d6h		;04ec	18 e8 	. .
l04eeh:
	cp 058h		;04ee	fe 58 	. X
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
	cp 059h		;051b	fe 59 	. Y
	jr nz,l053fh		;051d	20 20 	   
l051fh:
	call sub_07ach		;051f	cd ac 07 	. . .
	cp 00dh		;0522	fe 0d 	. .
	jr nz,l051fh		;0524	20 f9 	  .
l0526h:
	call sub_07ach		;0526	cd ac 07 	. . .
	cp 00dh		;0529	fe 0d 	. .
	jp z,l05f8h		;052b	ca f8 05 	. . .
	and 00fh		;052e	e6 0f 	. .
	rlca			;0530	07 	.
	rlca			;0531	07 	.
	rlca			;0532	07 	.
	rlca			;0533	07 	.
	ld d,a			;0534	57 	W
	call sub_07ach		;0535	cd ac 07 	. . .
	and 00fh		;0538	e6 0f 	. .
	or d			;053a	b2 	.
	ld (hl),a			;053b	77 	w
	inc hl			;053c	23 	#
	jr l0526h		;053d	18 e7 	. .
l053fh:
	cp 05ah		;053f	fe 5a 	. Z
	jp nz,l0635h		;0541	c2 35 06 	. 5 .
	call sub_09b5h		;0544	cd b5 09 	. . .
	xor a			;0547	af 	.
	ld (0fd1dh),a		;0548	32 1d fd 	2 . .
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
	call sub_061ch		;057a	cd 1c 06 	. . .
	ld ix,l090eh		;057d	dd 21 0e 09 	. ! . .
	call sub_09a0h		;0581	cd a0 09 	. . .
	ld a,(0fd15h)		;0584	3a 15 fd 	: . .
	and 0c0h		;0587	e6 c0 	. .
	call nz,sub_0913h		;0589	c4 13 09 	. . .
	ld a,h			;058c	7c 	|
	or a			;058d	b7 	.
	jr nz,l0579h		;058e	20 e9 	  .
l0590h:
	push hl			;0590	e5 	.
	call sub_061ch		;0591	cd 1c 06 	. . .
	ld ix,l090eh		;0594	dd 21 0e 09 	. ! . .
	call sub_0997h		;0598	cd 97 09 	. . .
	ld a,(0fd15h)		;059b	3a 15 fd 	: . .
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
	ld a,(0fd15h)		;05bc	3a 15 fd 	: . .
	and 0c0h		;05bf	e6 c0 	. .
	ld (0fd15h),a		;05c1	32 15 fd 	2 . .
	ld (0fd16h),a		;05c4	32 16 fd 	2 . .
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
	ld a,(0fd80h)		;05f8	3a 80 fd 	: . .
	or a			;05fb	b7 	.
	jr z,l0612h		;05fc	28 14 	( .
	cp 080h		;05fe	fe 80 	. .
	jr z,l0612h		;0600	28 10 	( .
	ld a,080h		;0602	3e 80 	> .
	ld (0fd80h),a		;0604	32 80 fd 	2 . .
	ld hl,0fd86h		;0607	21 86 fd 	! . .
	ld de,0fd8bh		;060a	11 8b fd 	. . .
	ld bc,00005h		;060d	01 05 00 	. . .
	ldir		;0610	ed b0 	. .
l0612h:
	jp l062fh		;0612	c3 2f 06 	. / .
l0615h:
	ld c,03fh		;0615	0e 3f 	. ?
	call sub_07e1h		;0617	cd e1 07 	. . .
	jr l062fh		;061a	18 13 	. .
sub_061ch:
	call sub_079ah		;061c	cd 9a 07 	. . .
	ret z			;061f	c8 	.
l0620h:
	ld ix,l0627h		;0620	dd 21 27 06 	. ! ' .
	jp l076fh		;0624	c3 6f 07 	. o .
l0627h:
	ld a,000h		;0627	3e 00 	> .
	ld (0fd80h),a		;0629	32 80 fd 	2 . .
	ld (0fd81h),a		;062c	32 81 fd 	2 . .
l062fh:
	ld sp,STACK_BASE		;062f	31 00 fe 	1 . .
	jp l0104h		;0632	c3 04 01 	. . .
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
	ld (0fd88h),hl		;0650	22 88 fd 	" . .
	ld (0fd86h),de		;0653	ed 53 86 fd 	. S . .
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
	ret			;0679	c9 	.
sub_067ah:
	push bc			;067a	c5 	.
l067bh:
	push bc			;067b	c5 	.
l067ch:
	djnz l067ch		;067c	10 fe 	. .
	pop bc			;067e	c1 	.
	djnz l067bh		;067f	10 fa 	. .
	pop bc			;0681	c1 	.
	djnz sub_067ah		;0682	10 f6 	. .
	ret			;0684	c9 	.
sub_0685h:
	call sub_07dfh		;0685	cd df 07 	. . .
	call sub_0692h		;0688	cd 92 06 	. . .
	ld c,02dh		;068b	0e 2d 	. -
	call sub_07e1h		;068d	cd e1 07 	. . .
	ld h,d			;0690	62 	b
	ld l,e			;0691	6b 	k
sub_0692h:
	call sub_0696h		;0692	cd 96 06 	. . .
	ld h,l			;0695	65 	e
sub_0696h:
	pop iy		;0696	fd e1 	. .
l0698h:
	ld a,h			;0698	7c 	|
	rlca			;0699	07 	.
	rlca			;069a	07 	.
	rlca			;069b	07 	.
	rlca			;069c	07 	.
	ld ix,l06a3h		;069d	dd 21 a3 06 	. ! . .
	jr l06ach		;06a1	18 09 	. .
l06a3h:
	ld a,h			;06a3	7c 	|
	ld ix,l06aah		;06a4	dd 21 aa 06 	. ! . .
	jr l06ach		;06a8	18 02 	. .
l06aah:
	jp (iy)		;06aa	fd e9 	. .
l06ach:
	and 00fh		;06ac	e6 0f 	. .
	add a,030h		;06ae	c6 30 	. 0
	cp 03ah		;06b0	fe 3a 	. :
	jp m,l06b7h		;06b2	fa b7 06 	. . .
	add a,007h		;06b5	c6 07 	. .
l06b7h:
	ld c,a			;06b7	4f 	O
	jp l07e3h		;06b8	c3 e3 07 	. . .
sub_06bbh:
	ld b,004h		;06bb	06 04 	. .
l06bdh:
	call sub_06cbh		;06bd	cd cb 06 	. . .
	djnz l06bdh		;06c0	10 fb 	. .
	ld a,c			;06c2	79 	y
	cp 00fh		;06c3	fe 0f 	. .
	ld a,042h		;06c5	3e 42 	> B
	ret nz			;06c7	c0 	.
	ld a,0c0h		;06c8	3e c0 	> .
	ret			;06ca	c9 	.
sub_06cbh:
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

;	Seems to be loading the virtual data map pointed by HL
;	Pages are 8K
l0711h:
	ld bc,DMAP		; b = 0, c = DMAP
l0714h:
	rld
	out (c),a
	ld a,b
	add a,020h		;0719	c6 20 	.  
	ld b,a			;071b	47 	G
	rrd		;071c	ed 67 	. g
	out (c),a		;071e	ed 79 	. y
	ld a,b			;0720	78 	x
	add a,020h		;0721	c6 20 	.  
	ld b,a			;0723	47 	G
	inc hl			;0724	23 	#
	or a			;0725	b7 	.
	jr nz,l0714h		;0726	20 ec 	  .
	jp (ix)		;0728	dd e9 	. .
sub_072ah:
	pop iy		;072a	fd e1 	. .
DISP_HL:
	ld c,(hl)			;072c	4e 	N
	ld b,c			;072d	41 	A
	res 7,c		;072e	cb b9 	. .
	inc hl			;0730	23 	#
	ld ix,l0738h		;0731	dd 21 38 07 	. ! 8 .
	jp l07e3h		;0735	c3 e3 07 	. . .
l0738h:
	bit 7,b		;0738	cb 78 	. x
	jr z,DISP_HL		;073a	28 f0 	( .
	jp (iy)		;073c	fd e9 	. .
	pop ix		;073e	dd e1 	. .
l0740h:
	ld a,0cfh		;0740	3e cf 	> .
	di			;0742	f3 	.
	out (SPIOA),a		;0743	d3 f9 	. .
	xor a			;0745	af 	.
	out (SPIOA),a		;0746	d3 f9 	. .
	ld a,0e3h		;0748	3e e3 	> .
	out (DPIOB),a		;074a	d3 fa 	. .
	ld bc,l0ef8h		;074c	01 f8 0e 	. . .
l074fh:
	dec b			;074f	05 	.
	out (c),b		;0750	ed 41 	. A
	inc b			;0752	04 	.
	ld a,0c3h		;0753	3e c3 	> .
	out (DPIOB),a		;0755	d3 fa 	. .
	ld a,0e3h		;0757	3e e3 	> .
	out (DPIOB),a		;0759	d3 fa 	. .
	outi		;075b	ed a3 	. .
	ld a,0e7h		;075d	3e e7 	> .
	out (DPIOB),a		;075f	d3 fa 	. .
	ld a,0c7h		;0761	3e c7 	> .
	out (DPIOB),a		;0763	d3 fa 	. .
	ld a,0e7h		;0765	3e e7 	> .
	out (DPIOB),a		;0767	d3 fa 	. .
	jr nz,l074fh		;0769	20 e4 	  .
	jp (ix)		;076b	dd e9 	. .
sub_076dh:
	pop ix		;076d	dd e1 	. .
l076fh:
	ld iy,l0776h		;076f	fd 21 76 07 	. ! v .
	jp l081ah		;0773	c3 1a 08 	. . .
l0776h:
	jr z,l0781h		;0776	28 09 	( .
	xor a			;0778	af 	.
	ld (0fd79h),a		;0779	32 79 fd 	2 y .
	ld a,(0fd78h)		;077c	3a 78 fd 	: x .
	jr l078ch		;077f	18 0b 	. .
l0781h:
	ld iy,l0788h		;0781	fd 21 88 07 	. ! . .
	jp l0790h		;0785	c3 90 07 	. . .
l0788h:
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
sub_079ah:
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
sub_07ach:
	ld a,(0fd79h)		;07ac	3a 79 fd 	: y .
	or a			;07af	b7 	.
	jp nz,l0620h		;07b0	c2 20 06 	.   .
	ld a,(0fd85h)		;07b3	3a 85 fd 	: . .
	and 011h		;07b6	e6 11 	. .
	jp z,l0615h		;07b8	ca 15 06 	. . .
	bit 0,a		;07bb	cb 47 	. G
	jr z,l07c7h		;07bd	28 08 	( .
	call sub_078eh		;07bf	cd 8e 07 	. . .
	jr z,l07c7h		;07c2	28 03 	( .
	in a,(DCOMM)		;07c4	db f0 	. .
	ret			;07c6	c9 	.
l07c7h:
	ld a,(0fd85h)		;07c7	3a 85 fd 	: . .
	bit 4,a		;07ca	cb 67 	. g
	jr z,sub_07ach		;07cc	28 de 	( .
	ld a,010h		;07ce	3e 10 	> .
	out (SPRTR),a		;07d0	d3 f3 	. .
	in a,(SPRTR)		;07d2	db f3 	. .
	and 001h		;07d4	e6 01 	. .
	jr z,sub_07ach		;07d6	28 d4 	( .
	in a,(DPRTR)		;07d8	db f2 	. .
	ret			;07da	c9 	.
sub_07dbh:
	pop ix		;07db	dd e1 	. .
	jr l07ebh		;07dd	18 0c 	. .
sub_07dfh:
	ld c,020h		;07df	0e 20 	.  
sub_07e1h:
	pop ix		;07e1	dd e1 	. .
l07e3h:
	ld a,i		;07e3	ed 57 	. W
	or a			;07e5	b7 	.
	jr z,l07f2h		;07e6	28 0a 	( .
	call l0abah		;07e8	cd ba 0a 	. . .
l07ebh:
	ld a,(0fd85h)		;07eb	3a 85 fd 	: . .
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
	ld a,(0fd85h)		;0801	3a 85 fd 	: . .
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
	jp (ix)		;0818	dd e9 	. .
l081ah:
	ld a,i		;081a	ed 57 	. W
	or a			;081c	b7 	.
	jr z,l0823h		;081d	28 04 	( .
	ld a,(0fd79h)		;081f	3a 79 fd 	: y .
	or a			;0822	b7 	.
l0823h:
	jp (iy)		;0823	fd e9 	. .
l0825h:
	xor a			;0825	af 	.
	ld (0fd79h),a		;0826	32 79 fd 	2 y .
l0829h:
	call sub_078eh		;0829	cd 8e 07 	. . .
	jr z,l0836h		;082c	28 08 	( .
	in a,(DCOMM)		;082e	db f0 	. .
	and 07fh		;0830	e6 7f 	. 
	ld c,a			;0832	4f 	O
	call sub_07e1h		;0833	cd e1 07 	. . .
l0836h:
	ld a,(0fd79h)		;0836	3a 79 fd 	: y .
	or a			;0839	b7 	.
	jr z,l0829h		;083a	28 ed 	( .
	ld a,(0fd78h)		;083c	3a 78 fd 	: x .
	ld c,a			;083f	4f 	O
	cp 086h		;0840	fe 86 	. .
	jr nz,l0850h		;0842	20 0c 	  .
	di			;0844	f3 	.
	ld sp,STACK_BASE		;0845	31 00 fe 	1 . .
	xor a			;0848	af 	.
	ld (0fd79h),a		;0849	32 79 fd 	2 y .
	ei			;084c	fb 	.
	jp l0104h		;084d	c3 04 01 	. . .
l0850h:
	ld a,010h		;0850	3e 10 	> .
	out (SCOMM),a		;0852	d3 f1 	. .
	in a,(SCOMM)		;0854	db f1 	. .
	and 004h		;0856	e6 04 	. .
	jr z,l0829h		;0858	28 cf 	( .
	di			;085a	f3 	.
	xor a			;085b	af 	.
	ld (0fd79h),a		;085c	32 79 fd 	2 y .
	ei			;085f	fb 	.
	ld a,c			;0860	79 	y
	out (DCOMM),a		;0861	d3 f0 	. .
	jr l0829h		;0863	18 c4 	. .
l0865h:
	push hl			;0865	e5 	.
	ld hl,(0fd20h)		;0866	2a 20 fd 	*   .
	ld a,h			;0869	7c 	|
	or l			;086a	b5 	.
	push af			;086b	f5 	.
	ld a,(0fd82h)		;086c	3a 82 fd 	: . .
	set 0,a		;086f	cb c7 	. .
	ld bc,0fbfah		;0871	01 fa fb 	. . .
	push af			;0874	f5 	.
	ld a,0cfh		;0875	3e cf 	> .
	di			;0877	f3 	.
	out (SPIOA),a		;0878	d3 f9 	. .
	xor a			;087a	af 	.
	out (SPIOA),a		;087b	d3 f9 	. .
	pop af			;087d	f1 	.
	out (DPIOA),a		;087e	d3 f8 	. .
	out (c),b		;0880	ed 41 	. A
	res 5,b		;0882	cb a8 	. .
	out (c),b		;0884	ed 41 	. A
	set 5,b		;0886	cb e8 	. .
	out (c),b		;0888	ed 41 	. A
	ei			;088a	fb 	.
	ld hl,l012ch		;088b	21 2c 01 	! , .
	ld (0fd20h),hl		;088e	22 20 fd 	"   .
	pop af			;0891	f1 	.
	jr nz,l08a0h		;0892	20 0c 	  .
	scf			;0894	37 	7
	ld de,00100h		;0895	11 00 01 	. . .
l0898h:
	ld hl,(0fd20h)		;0898	2a 20 fd 	*   .
	sbc hl,de		;089b	ed 52 	. R
	jp p,l0898h		;089d	f2 98 08 	. . .
l08a0h:
	pop hl			;08a0	e1 	.
	ld d,h			;08a1	54 	T
	ld e,l			;08a2	5d 	]
	bit 4,(hl)		;08a3	cb 66 	. f
	jr z,l08c6h		;08a5	28 1f 	( .
	ld a,044h		;08a7	3e 44 	> D
	bit 6,(hl)		;08a9	cb 76 	. v
	jr z,l08afh		;08ab	28 02 	( .
	ld a,048h		;08ad	3e 48 	> H
l08afh:
	out (DMAWMR),a		;08af	d3 eb 	. .
	push hl			;08b1	e5 	.
	inc hl			;08b2	23 	#
	ld c,0e0h		;08b3	0e e0 	. .
	outi		;08b5	ed a3 	. .
	outi		;08b7	ed a3 	. .
	ld c,0e1h		;08b9	0e e1 	. .
	outi		;08bb	ed a3 	. .
	outi		;08bd	ed a3 	. .
	ld a,000h		;08bf	3e 00 	> .
	out (DMAWSM),a		;08c1	d3 ea 	. .
	dec hl			;08c3	2b 	+
	ex de,hl			;08c4	eb 	.
	pop hl			;08c5	e1 	.
l08c6h:
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
	call sub_0a8fh		;08d0	cd 8f 0a 	. . .
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
	call sub_0aa5h		;08e6	cd a5 0a 	. . .
	ld (0fd1ch),a		;08e9	32 1c fd 	2 . .
	ld a,002h		;08ec	3e 02 	> .
	ret nz			;08ee	c0 	.
	pop bc			;08ef	c1 	.
	jp (ix)		;08f0	dd e9 	. .
l08f2h:
	ld b,002h		;08f2	06 02 	. .
l08f4h:
	ld hl,0a000h		;08f4	21 00 a0 	! . .
l08f7h:
	ld a,(0fd1eh)		;08f7	3a 1e fd 	: . .
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
	ld (0fd1eh),a		;090a	32 1e fd 	2 . .
	ret			;090d	c9 	.
l090eh:
	call sub_0913h		;090e	cd 13 09 	. . .
	jr l092ah		;0911	18 17 	. .
sub_0913h:
	ld a,(0fd15h)		;0913	3a 15 fd 	: . .
	ld d,a			;0916	57 	W
	ld a,(0fd16h)		;0917	3a 16 fd 	: . .
	ld e,a			;091a	5f 	_
	push hl			;091b	e5 	.
	call sub_0685h		;091c	cd 85 06 	. . .
	pop hl			;091f	e1 	.
	ld a,(0fd1dh)		;0920	3a 1d fd 	: . .
	inc a			;0923	3c 	<
	ld (0fd1dh),a		;0924	32 1d fd 	2 . .
	cp 017h		;0927	fe 17 	. .
	ret m			;0929	f8 	.
l092ah:
	jp l062fh		;092a	c3 2f 06 	. / .
sub_092dh:
	push hl			;092d	e5 	.
	ld hl,l0f0ah		;092e	21 0a 0f 	! . .
	ld de,0fd00h		;0931	11 00 fd 	. . .
	ld bc,0015h		;0934	01 15 00 	. . .
	ldir		;0937	ed b0 	. .
	pop hl			;0939	e1 	.
	bit 6,l		;093a	cb 75 	. u
	jr z,l0943h		;093c	28 05 	( .
	ld a,001h		;093e	3e 01 	> .
	ld (0fd08h),a		;0940	32 08 fd 	2 . .
l0943h:
	ld a,l			;0943	7d 	}
	rlca			;0944	07 	.
	rlca			;0945	07 	.
	rlca			;0946	07 	.
	rlca			;0947	07 	.
	and 005h		;0948	e6 05 	. .
	ld (0fd06h),a		;094a	32 06 fd 	2 . .
	ld (0fd13h),a		;094d	32 13 fd 	2 . .
	res 2,a		;0950	cb 97 	. .
	ld (0fd10h),a		;0952	32 10 fd 	2 . .
	ld a,(0fd1fh)		;0955	3a 1f fd 	: . .
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
	ld (0fd1fh),a		;096a	32 1f fd 	2 . .
	push hl			;096d	e5 	.
	ld hl,0fd0eh		;096e	21 0e fd 	! . .
	push hl			;0971	e5 	.
	call l0865h		;0972	cd 65 08 	. e .
	ld hl,0fd11h		;0975	21 11 fd 	! . .
	call l0865h		;0978	cd 65 08 	. e .
	pop hl			;097b	e1 	.
	call l0865h		;097c	cd 65 08 	. e .
	pop hl			;097f	e1 	.
l0980h:
	ld a,l			;0980	7d 	}
	and 00fh		;0981	e6 0f 	. .
	ld (0fd09h),a		;0983	32 09 fd 	2 . .
	ld a,h			;0986	7c 	|
	ld (0fd14h),a		;0987	32 14 fd 	2 . .
	ld (0fd07h),a		;098a	32 07 fd 	2 . .
	ld hl,0fd11h		;098d	21 11 fd 	! . .
	call l0865h		;0990	cd 65 08 	. e .
	ld hl,0fd00h		;0993	21 00 fd 	! . .
	ret			;0996	c9 	.
sub_0997h:
	push hl			;0997	e5 	.
	call sub_092dh		;0998	cd 2d 09 	. - .
	call l0865h		;099b	cd 65 08 	. e .
	pop hl			;099e	e1 	.
	ret			;099f	c9 	.
sub_09a0h:
	push hl			;09a0	e5 	.
	call sub_092dh		;09a1	cd 2d 09 	. - .
	ld (hl),079h		;09a4	36 79 	6 y
	ld a,045h		;09a6	3e 45 	> E
	ld (0fd05h),a		;09a8	32 05 fd 	2 . .
	call l0865h		;09ab	cd 65 08 	. e .
	pop hl			;09ae	e1 	.
sub_09afh:
	ld b,00dh		;09af	06 0d 	. .
	call sub_067ah		;09b1	cd 7a 06 	. z .
	ret			;09b4	c9 	.
sub_09b5h:
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
	ld hl,00f23h		;09e7	21 23 0f 	! # .
	ld bc,0000bh		;09ea	01 0b 00 	. . .
	ldir		;09ed	ed b0 	. .
	ld a,(0fd13h)		;09ef	3a 13 fd 	: . .
	ld (0fd06h),a		;09f2	32 06 fd 	2 . .
	pop hl			;09f5	e1 	.
	call sub_061ch		;09f6	cd 1c 06 	. . .
	ld ix,l090eh		;09f9	dd 21 0e 09 	. ! . .
	call l0865h		;09fd	cd 65 08 	. e .
	pop hl			;0a00	e1 	.
	call sub_09afh		;0a01	cd af 09 	. . .
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
l0a1bh:
	ld c,000h		;0a1b	0e 00 	. .
	call sub_0a80h		;0a1d	cd 80 0a 	. . .
	call l0909h		;0a20	cd 09 09 	. . .
	ld hl,l0f1fh		;0a23	21 1f 0f 	! . .
	call l0865h		;0a26	cd 65 08 	. e .
	ld a,003h		;0a29	3e 03 	> .
l0a2bh:
	ld (0fd7ch),sp		;0a2b	ed 73 7c fd 	. s | .
l0a2fh:
	ld (0fd7bh),a		;0a2f	32 7b fd 	2 { .
	ld ix,l0a56h		;0a32	dd 21 56 0a 	. ! V .
	ld hl,1		;0a36	21 01 00 	! . .
	call sub_0997h		;0a39	cd 97 09 	. . .
	ld hl,l0f84h		;0a3c	21 84 0f 	! . .
	ld a,(0fd15h)		;0a3f	3a 15 fd 	: . .
	and 0c0h		;0a42	e6 c0 	. .
	jr nz,l0a56h		;0a44	20 10 	  .
	ld a,(0fe03h)		;0a46	3a 03 fe 	: . .
	cp 0a7h		;0a49	fe a7 	. .
	jp z,STACK_BASE		;0a4b	ca 00 fe 	. . .
	ld a,(0fd7bh)		;0a4e	3a 7b fd 	: { .
	dec a			;0a51	3d 	=
	jr nz,l0a2fh		;0a52	20 db 	  .
	jr l0a6bh		;0a54	18 15 	. .
l0a56h:
	ld c,008h		;0a56	0e 08 	. .
	call sub_0a80h		;0a58	cd 80 0a 	. . .
	call sub_0aa5h		;0a5b	cd a5 0a 	. . .
	ld sp,(0fd7ch)		;0a5e	ed 7b 7c fd 	. { | .
	ld a,(0fd7bh)		;0a62	3a 7b fd 	: { .
	dec a			;0a65	3d 	=
	jr nz,l0a2bh		;0a66	20 c3 	  .
	ld hl,l0f66h		;0a68	21 66 0f 	! f .
l0a6bh:
	call sub_072ah		;0a6b	cd 2a 07 	. * .
l0a6eh:
	ld hl,00f97h		;0a6e	21 97 0f 	! . .
	call sub_072ah		;0a71	cd 2a 07 	. * .
	di			;0a74	f3 	.
	xor a			;0a75	af 	.
	ld (0fd79h),a		;0a76	32 79 fd 	2 y .
	ld sp,STACK_BASE		;0a79	31 00 fe 	1 . .
	ei			;0a7c	fb 	.
	jp l0825h		;0a7d	c3 25 08 	. % .
sub_0a80h:
	ld b,00ah		;0a80	06 0a 	. .
l0a82h:
	push bc			;0a82	c5 	.
	call sub_0aa5h		;0a83	cd a5 0a 	. . .
	pop bc			;0a86	c1 	.
	djnz l0a82h		;0a87	10 f9 	. .
	call sub_0a8fh		;0a89	cd 8f 0a 	. . .
	jp sub_0aa5h		;0a8c	c3 a5 0a 	. . .
sub_0a8fh:
	ld b,01fh		;0a8f	06 1f 	. .
l0a91h:
	djnz l0a91h		;0a91	10 fe 	. .
	ld b,00ah		;0a93	06 0a 	. .
l0a95h:
	in a,(0fch)		;0a95	db fc 	. .
	and 0c0h		;0a97	e6 c0 	. .
	cp 080h		;0a99	fe 80 	. .
	jr z,l0aa0h		;0a9b	28 03 	( .
	djnz l0a95h		;0a9d	10 f6 	. .
	ret			;0a9f	c9 	.
l0aa0h:
	or a			;0aa0	b7 	.
	ld a,c			;0aa1	79 	y
	out (DFLPY),a		;0aa2	d3 fd 	. .
	ret			;0aa4	c9 	.
sub_0aa5h:
	ld b,01fh		;0aa5	06 1f 	. .
l0aa7h:
	djnz l0aa7h		;0aa7	10 fe 	. .
	ld b,00ah		;0aa9	06 0a 	. .
l0aabh:
	in a,(0fch)		;0aab	db fc 	. .
	and 0c0h		;0aad	e6 c0 	. .
	cp 0c0h		;0aaf	fe c0 	. .
	jr z,l0ab6h		;0ab1	28 03 	( .
	djnz l0aabh		;0ab3	10 f6 	. .
	ret			;0ab5	c9 	.
l0ab6h:
	or a			;0ab6	b7 	.
	in a,(DFLPY)		;0ab7	db fd 	. .
	ret			;0ab9	c9 	.
l0abah:
	push hl			;0aba	e5 	.
	push de			;0abb	d5 	.
	push bc			;0abc	c5 	.
	push af			;0abd	f5 	.
l0abeh:
	ld hl,(0fd22h)		;0abe	2a 22 fd 	* " .
	ld a,(0fd77h)		;0ac1	3a 77 fd 	: w .
	add a,l			;0ac4	85 	.
	cp 018h		;0ac5	fe 18 	. .
	jr c,l0acbh		;0ac7	38 02 	8 .
	sbc a,018h		;0ac9	de 18 	. .
l0acbh:
	ld l,a			;0acb	6f 	o
	ld a,(0fd76h)		;0acc	3a 76 fd 	: v .
	or a			;0acf	b7 	.
	jp nz,l0bbfh		;0ad0	c2 bf 0b 	. . .
	ld a,c			;0ad3	79 	y
	and 060h		;0ad4	e6 60 	. `
	jr z,l0b36h		;0ad6	28 5e 	( ^
	ld a,c			;0ad8	79 	y
	cp 040h		;0ad9	fe 40 	. @
	jr c,l0af3h		;0adb	38 16 	8 .
	cp 061h		;0add	fe 61 	. a
	jr nc,l0af3h		;0adf	30 12 	0 .
	and 03fh		;0ae1	e6 3f 	. ?
	push af			;0ae3	f5 	.
	ld de,00f37h		;0ae4	11 37 0f 	. 7 .
	ex de,hl			;0ae7	eb 	.
	ld a,(0fd25h)		;0ae8	3a 25 fd 	: % .
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
	out (SDSPY),a		;0af7	d3 ee 	. .
	ld b,h			;0af9	44 	D
	ld c,0feh		;0afa	0e fe 	. .
	out (c),e		;0afc	ed 59 	. Y
	and 0dfh		;0afe	e6 df 	. .
	out (SDSPY),a		;0b00	d3 ee 	. .
	and 01fh		;0b02	e6 1f 	. .
	ld l,a			;0b04	6f 	o
	ld a,(0fd24h)		;0b05	3a 24 fd 	: $ .
	out (c),a		;0b08	ed 79 	. y
	ld a,h			;0b0a	7c 	|
	cp 04fh		;0b0b	fe 4f 	. O
	jr nz,l0b17h		;0b0d	20 08 	  .
l0b0fh:
	xor a			;0b0f	af 	.
	ld (0fd23h),a		;0b10	32 23 fd 	2 # .
	ld c,00ah		;0b13	0e 0a 	. .
	jr l0abeh		;0b15	18 a7 	. .
l0b17h:
	inc a			;0b17	3c 	<
l0b18h:
	ld h,a			;0b18	67 	g
	ld (0fd23h),a		;0b19	32 23 fd 	2 # .
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
l0b2dh:
	xor a			;0b2d	af 	.
l0b2eh:
	ld (0fd76h),a		;0b2e	32 76 fd 	2 v .
l0b31h:
	pop af			;0b31	f1 	.
	pop bc			;0b32	c1 	.
	pop de			;0b33	d1 	.
	pop hl			;0b34	e1 	.
	ret			;0b35	c9 	.
l0b36h:
	ld a,c			;0b36	79 	y
	cp 01bh		;0b37	fe 1b 	. .
	jr nz,l0b40h		;0b39	20 05 	  .
	ld a,001h		;0b3b	3e 01 	> .
	jp l0b2eh		;0b3d	c3 2e 0b 	. . .
l0b40h:
	cp 008h		;0b40	fe 08 	. .
	jr nz,l0b4ch		;0b42	20 08 	  .
	ld a,h			;0b44	7c 	|
	or a			;0b45	b7 	.
	jr z,l0b2dh		;0b46	28 e5 	( .
	dec a			;0b48	3d 	=
	jp l0b18h		;0b49	c3 18 0b 	. . .
l0b4ch:
	cp 00dh		;0b4c	fe 0d 	. .
	jr nz,l0b54h		;0b4e	20 04 	  .
	xor a			;0b50	af 	.
	jp l0b18h		;0b51	c3 18 0b 	. . .
l0b54h:
	cp 00ah		;0b54	fe 0a 	. .
	jr nz,l0b91h		;0b56	20 39 	  9
	ld a,(0fd22h)		;0b58	3a 22 fd 	: " .
	cp 017h		;0b5b	fe 17 	. .
	jr z,l0b6dh		;0b5d	28 0e 	( .
	inc a			;0b5f	3c 	<
	ld (0fd22h),a		;0b60	32 22 fd 	2 " .
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
	ld a,(0fd77h)		;0b73	3a 77 fd 	: w .
	inc a			;0b76	3c 	<
	cp 018h		;0b77	fe 18 	. .
	jr nz,l0b7ch		;0b79	20 01 	  .
	xor a			;0b7b	af 	.
l0b7ch:
	ld (0fd77h),a		;0b7c	32 77 fd 	2 w .
	ld a,l			;0b7f	7d 	}
	inc a			;0b80	3c 	<
	inc l			;0b81	2c 	,
	cp 018h		;0b82	fe 18 	. .
	jr nz,l0b88h		;0b84	20 02 	  .
	ld l,000h		;0b86	2e 00 	. .
l0b88h:
	call sub_0c7ah		;0b88	cd 7a 0c 	. z .
	ld a,(0fd23h)		;0b8b	3a 23 fd 	: # .
	ld h,a			;0b8e	67 	g
	jr l0b1ch		;0b8f	18 8b 	. .
l0b91h:
	cp 007h		;0b91	fe 07 	. .
	jr nz,l0ba3h		;0b93	20 0e 	  .
	ld hl,00fdeh		;0b95	21 de 0f 	! . .
	ld ix,l0b9fh		;0b98	dd 21 9f 0b 	. ! . .
	jp l0740h		;0b9c	c3 40 07 	. @ .
l0b9fh:
	ei			;0b9f	fb 	.
	jp l0b2dh		;0ba0	c3 2d 0b 	. - .
l0ba3h:
	cp 01eh		;0ba3	fe 1e 	. .
	jr nz,l0bb2h		;0ba5	20 0b 	  .
l0ba7h:
	ld hl,1		;0ba7	21 01 00 	! . .
	ld (0fd22h),hl		;0baa	22 22 fd 	" " .
	ld c,008h		;0bad	0e 08 	. .
	jp l0abeh		;0baf	c3 be 0a 	. . .
l0bb2h:
	cp 00bh		;0bb2	fe 0b 	. .
	jp z,l0bd7h		;0bb4	ca d7 0b 	. . .
	cp 00ch		;0bb7	fe 0c 	. .
	jp nz,l0b31h		;0bb9	c2 31 0b 	. 1 .
	jp l0c08h		;0bbc	c3 08 0c 	. . .
l0bbfh:
	dec a			;0bbf	3d 	=
	jp nz,l0c55h		;0bc0	c2 55 0c 	. U .
	ld a,c			;0bc3	79 	y
	cp 03dh		;0bc4	fe 3d 	. =
	jr nz,l0bcfh		;0bc6	20 07 	  .
l0bc8h:
	ld hl,0fd76h		;0bc8	21 76 fd 	! v .
	inc (hl)			;0bcb	34 	4
	jp l0b31h		;0bcc	c3 31 0b 	. 1 .
l0bcfh:
	cp 059h		;0bcf	fe 59 	. Y
	jr z,l0bc8h		;0bd1	28 f5 	( .
	cp 041h		;0bd3	fe 41 	. A
	jr nz,l0bebh		;0bd5	20 14 	  .
l0bd7h:
	ld a,(0fd22h)		;0bd7	3a 22 fd 	: " .
	or a			;0bda	b7 	.
	jp z,l0b2dh		;0bdb	ca 2d 0b 	. - .
	dec a			;0bde	3d 	=
	dec l			;0bdf	2d 	-
	jp p,l0be5h		;0be0	f2 e5 0b 	. . .
	ld l,017h		;0be3	2e 17 	. .
l0be5h:
	ld (0fd22h),a		;0be5	32 22 fd 	2 " .
	jp l0b1ch		;0be8	c3 1c 0b 	. . .
l0bebh:
	cp 042h		;0beb	fe 42 	. B
	jr nz,l0c04h		;0bed	20 15 	  .
	ld a,(0fd22h)		;0bef	3a 22 fd 	: " .
	cp 017h		;0bf2	fe 17 	. .
	jp z,l0b2dh		;0bf4	ca 2d 0b 	. - .
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
	inc a			;0c0e	3c 	<
	jp l0b18h		;0c0f	c3 18 0b 	. . .
l0c12h:
	cp 044h		;0c12	fe 44 	. D
	jr nz,l0c1fh		;0c14	20 09 	  .
	ld a,h			;0c16	7c 	|
	or a			;0c17	b7 	.
	jp z,l0b2dh		;0c18	ca 2d 0b 	. - .
	dec a			;0c1b	3d 	=
	jp l0b18h		;0c1c	c3 18 0b 	. . .
l0c1fh:
	cp 04ah		;0c1f	fe 4a 	. J
	jr nz,l0c43h		;0c21	20 20 	   
	call sub_0c9fh		;0c23	cd 9f 0c 	. . .
	ld a,(0fd22h)		;0c26	3a 22 fd 	: " .
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
	jp l0b2dh		;0c40	c3 2d 0b 	. - .
l0c43h:
	cp 04bh		;0c43	fe 4b 	. K
	jr nz,l0c4dh		;0c45	20 06 	  .
	call sub_0c9fh		;0c47	cd 9f 0c 	. . .
	jp l0b1ch		;0c4a	c3 1c 0b 	. . .
l0c4dh:
	cp 048h		;0c4d	fe 48 	. H
	jp z,l0ba7h		;0c4f	ca a7 0b 	. . .
	jp l0b2dh		;0c52	c3 2d 0b 	. - .
l0c55h:
	dec a			;0c55	3d 	=
	jr nz,l0c6bh		;0c56	20 13 	  .
	ld a,003h		;0c58	3e 03 	> .
	ld (0fd76h),a		;0c5a	32 76 fd 	2 v .
	ld a,c			;0c5d	79 	y
	sbc a,020h		;0c5e	de 20 	.  
	cp 018h		;0c60	fe 18 	. .
	jp p,l0b31h		;0c62	f2 31 0b 	. 1 .
	ld (0fd22h),a		;0c65	32 22 fd 	2 " .
	jp l0b31h		;0c68	c3 31 0b 	. 1 .
l0c6bh:
	dec a			;0c6b	3d 	=
	jp nz,l0b2dh		;0c6c	c2 2d 0b 	. - .
	ld a,c			;0c6f	79 	y
	sbc a,020h		;0c70	de 20 	.  
	cp 050h		;0c72	fe 50 	. P
	jp p,l0b2dh		;0c74	f2 2d 0b 	. - .
	jp l0b18h		;0c77	c3 18 0b 	. . .
sub_0c7ah:
	ld h,000h		;0c7a	26 00 	& .
	ld a,l			;0c7c	7d 	}
	or 0c0h		;0c7d	f6 c0 	. .
	ld l,a			;0c7f	6f 	o
	ld de,0020h		;0c80	11 20 00 	.   .
	ld bc,04feeh		;0c83	01 ee 4f 	. . O
	push bc			;0c86	c5 	.
	out (c),l		;0c87	ed 69 	. i
	ld c,0feh		;0c89	0e fe 	. .
l0c8bh:
	out (c),d		;0c8b	ed 51 	. Q
	djnz l0c8bh		;0c8d	10 fc 	. .
	out (c),d		;0c8f	ed 51 	. Q
	pop bc			;0c91	c1 	.
	set 5,l		;0c92	cb ed 	. .
	out (c),l		;0c94	ed 69 	. i
	ld c,0feh		;0c96	0e fe 	. .
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
	ld c,0feh		;0caf	0e fe 	. .
	out (c),d		;0cb1	ed 51 	. Q
	ld c,0eeh		;0cb3	0e ee 	. .
	set 5,l		;0cb5	cb ed 	. .
	out (c),l		;0cb7	ed 69 	. i
	ld c,0feh		;0cb9	0e fe 	. .
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
	ld c,0eeh		;0cd7	0e ee 	. .
	out (c),d		;0cd9	ed 51 	. Q
	ld hl,0fd26h		;0cdb	21 26 fd 	! & .
	push hl			;0cde	e5 	.
	push bc			;0cdf	c5 	.
	ld c,0feh		;0ce0	0e fe 	. .
	inir		;0ce2	ed b2 	. .
	pop bc			;0ce4	c1 	.
	pop hl			;0ce5	e1 	.
	out (c),e		;0ce6	ed 59 	. Y
	ld c,0feh		;0ce8	0e fe 	. .
	otir		;0cea	ed b3 	. .
	ret			;0cec	c9 	.
	push hl			;0ced	e5 	.
	push de			;0cee	d5 	.
	push bc			;0cef	c5 	.
	push af			;0cf0	f5 	.
	in a,(DPIOB)		;0cf1	db fa 	. .
	bit 6,a		;0cf3	cb 77 	. w
	jr z,l0d49h		;0cf5	28 52 	( R
	ld hl,l0fd0h		;0cf7	21 d0 0f 	! . .
	push ix		;0cfa	dd e5 	. .
	ld ix,l0d03h		;0cfc	dd 21 03 0d 	. ! . .
	jp l0740h		;0d00	c3 40 07 	. @ .
l0d03h:
	pop ix		;0d03	dd e1 	. .
	ld b,009h		;0d05	06 09 	. .
l0d07h:
	in a,(DPIOB)		;0d07	db fa 	. .
	rlca			;0d09	07 	.
	rlca			;0d0a	07 	.
	ccf			;0d0b	3f 	?
	rr l		;0d0c	cb 1d 	. .
	ld a,07fh		;0d0e	3e 7f 	> 
	out (DPIOB),a		;0d10	d3 fa 	. .
	ld a,0ffh		;0d12	3e ff 	> .
	out (DPIOB),a		;0d14	d3 fa 	. .
	djnz l0d07h		;0d16	10 ef 	. .
	bit 6,l		;0d18	cb 75 	. u
	jr z,l0d1eh		;0d1a	28 02 	( .
	res 7,l		;0d1c	cb bd 	. .
l0d1eh:
	ld h,000h		;0d1e	26 00 	& .
	ld de,00e4ah		;0d20	11 4a 0e 	. J .
	add hl,de			;0d23	19 	.
	ld a,(hl)			;0d24	7e 	~
	cp 0feh		;0d25	fe fe 	. .
	ld a,(0fd7ah)		;0d27	3a 7a fd 	: z .
	jr nz,l0d32h		;0d2a	20 06 	  .
	cpl			;0d2c	2f 	/
	ld (0fd7ah),a		;0d2d	32 7a fd 	2 z .
	jr l0d49h		;0d30	18 17 	. .
l0d32h:
	or a			;0d32	b7 	.
	ld a,(hl)			;0d33	7e 	~
	jr z,l0d41h		;0d34	28 0b 	( .
	cp 061h		;0d36	fe 61 	. a
	jr c,l0d41h		;0d38	38 07 	8 .
	cp 07bh		;0d3a	fe 7b 	. {
	jp p,l0d41h		;0d3c	f2 41 0d 	. A .
	res 5,a		;0d3f	cb af 	. .
l0d41h:
	ld (0fd78h),a		;0d41	32 78 fd 	2 x .
	ld a,0ffh		;0d44	3e ff 	> .
	ld (0fd79h),a		;0d46	32 79 fd 	2 y .
l0d49h:
	ld hl,(0fd20h)		;0d49	2a 20 fd 	*   .
	ld a,h			;0d4c	7c 	|
	or l			;0d4d	b5 	.
	jr nz,l0d6fh		;0d4e	20 1f 	  .
	ld a,0cfh		;0d50	3e cf 	> .
	out (SPIOA),a		;0d52	d3 f9 	. .
	xor a			;0d54	af 	.
	ld (0fd1fh),a		;0d55	32 1f fd 	2 . .
	out (SPIOA),a		;0d58	d3 f9 	. .
	ld a,(0fd82h)		;0d5a	3a 82 fd 	: . .
	res 0,a		;0d5d	cb 87 	. .
	out (DPIOA),a		;0d5f	d3 f8 	. .
	ld bc,0fbfah		;0d61	01 fa fb 	. . .
	out (c),b		;0d64	ed 41 	. A
	res 5,b		;0d66	cb a8 	. .
	out (c),b		;0d68	ed 41 	. A
	set 5,b		;0d6a	cb e8 	. .
	out (c),b		;0d6c	ed 41 	. A
	inc hl			;0d6e	23 	#
l0d6fh:
	dec hl			;0d6f	2b 	+
	ld (0fd20h),hl		;0d70	22 20 fd 	"   .
l0d73h:
	pop af			;0d73	f1 	.
	pop bc			;0d74	c1 	.
	pop de			;0d75	d1 	.
	pop hl			;0d76	e1 	.
	ei			;0d77	fb 	.
	reti		;0d78	ed 4d 	. M
	push hl			;0d7a	e5 	.
	push de			;0d7b	d5 	.
	push bc			;0d7c	c5 	.
	push af			;0d7d	f5 	.
	ld a,0ffh		;0d7e	3e ff 	> .
	ld (0fd1eh),a		;0d80	32 1e fd 	2 . .
	in a,(0fch)		;0d83	db fc 	. .
	and 010h		;0d85	e6 10 	. .
	jr z,l0d99h		;0d87	28 10 	( .
	ld hl,0fd15h		;0d89	21 15 fd 	! . .
	ld b,007h		;0d8c	06 07 	. .
l0d8eh:
	push bc			;0d8e	c5 	.
	call sub_0aa5h		;0d8f	cd a5 0a 	. . .
	ld (hl),a			;0d92	77 	w
	inc hl			;0d93	23 	#
	pop bc			;0d94	c1 	.
	djnz l0d8eh		;0d95	10 f7 	. .
	jr l0d73h		;0d97	18 da 	. .
l0d99h:
	ld c,008h		;0d99	0e 08 	. .
	call sub_0a8fh		;0d9b	cd 8f 0a 	. . .
	call sub_0aa5h		;0d9e	cd a5 0a 	. . .
	jr nz,l0dabh		;0da1	20 08 	  .
	bit 7,a		;0da3	cb 7f 	. 
	jr z,l0dabh		;0da5	28 04 	( .
	bit 6,a		;0da7	cb 77 	. w
	jr z,l0d73h		;0da9	28 c8 	( .
l0dabh:
	ld (0fd15h),a		;0dab	32 15 fd 	2 . .
	call sub_0aa5h		;0dae	cd a5 0a 	. . .
	ld (0fd18h),a		;0db1	32 18 fd 	2 . .
	jr l0d73h		;0db4	18 bd 	. .

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


	db 008h

	add hl,bc			;0e4b	09 	.
	ld a,(bc)			;0e4c	0a 	.
	nop			;0e4d	00 	.
	nop			;0e4e	00 	.
	dec c			;0e4f	0d 	.
	nop			;0e50	00 	.
	cp 020h		;0e51	fe 20 	.  
	nop			;0e53	00 	.
	nop			;0e54	00 	.
	dec de			;0e55	1b 	.
	add a,c			;0e56	81 	.
	add a,d			;0e57	82 	.
	add a,e			;0e58	83 	.
	add a,h			;0e59	84 	.
	db '^!@#$%&*()'
	db 022h ; "
	db ':<+>?~ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}_'
	rlca			;0e89	07 	.
	ex af,af'			;0e8a	08 	.
	add hl,bc			;0e8b	09 	.
	add a,(hl)			;0e8c	86 	.
	nop			;0e8d	00 	.
	nop			;0e8e	00 	.
	dec c			;0e8f	0d 	.
	nop			;0e90	00 	.
	cp 020h		;0e91	fe 20 	.  
	nop			;0e93	00 	.
	nop			;0e94	00 	.
	dec de			;0e95	1b 	.
	add a,c			;0e96	81 	.
	add a,d			;0e97	82 	.
	add a,e			;0e98	83 	.
	add a,h			;0e99	84 	.
	db '0123456789:;,=./'
	nop			;0eaa	00 	.
	ld bc,00302h		;0eab	01 02 03 	. . .
	inc b			;0eae	04 	.
	dec b			;0eaf	05 	.
	ld b,007h		;0eb0	06 07 	. .
	ex af,af'			;0eb2	08 	.
	add hl,bc			;0eb3	09 	.
	ld a,(bc)			;0eb4	0a 	.
	dec bc			;0eb5	0b 	.
	inc c			;0eb6	0c 	.
	dec c			;0eb7	0d 	.
	ld c,00fh		;0eb8	0e 0f 	. .
	djnz l0ecdh		;0eba	10 11 	. .
	ld (de),a			;0ebc	12 	.
	inc de			;0ebd	13 	.
	inc d			;0ebe	14 	.
	dec d			;0ebf	15 	.
	ld d,017h		;0ec0	16 17 	. .
	jr $+27		;0ec2	18 19 	. .
	ld a,(de)			;0ec4	1a 	.
	dec de			;0ec5	1b 	.
	inc e			;0ec6	1c 	.
	dec e			;0ec7	1d 	.
	ld e,01fh		;0ec8	1e 1f 	. .
	ex af,af'			;0eca	08 	.
	add hl,bc			;0ecb	09 	.
	ld a,(bc)			;0ecc	0a 	.
l0ecdh:
	nop			;0ecd	00 	.
	nop			;0ece	00 	.
	dec c			;0ecf	0d 	.
	nop			;0ed0	00 	.
	cp 020h		;0ed1	fe 20 	.  
	nop			;0ed3	00 	.
	nop			;0ed4	00 	.
	dec de			;0ed5	1b 	.
	add a,c			;0ed6	81 	.
	add a,d			;0ed7	82 	.
	add a,e			;0ed8	83 	.
	add a,h			;0ed9	84 	.
	jr nc,$+51		;0eda	30 31 	0 1
	ld (03433h),a		;0edc	32 33 34 	2 3 4
	dec (hl)			;0edf	35 	5
	ld (hl),037h		;0ee0	36 37 	6 7
	jr c,l0f1dh		;0ee2	38 39 	8 9
	daa			;0ee4	27 	'
	dec sp			;0ee5	3b 	;
	inc l			;0ee6	2c 	,
	dec a			;0ee7	3d 	=
	ld l,02fh		;0ee8	2e 2f 	. /
	ld h,b			;0eea	60 	`
	ld h,c			;0eeb	61 	a
	ld h,d			;0eec	62 	b
	ld h,e			;0eed	63 	c
	ld h,h			;0eee	64 	d
	ld h,l			;0eef	65 	e
	ld h,(hl)			;0ef0	66 	f
	ld h,a			;0ef1	67 	g
	ld l,b			;0ef2	68 	h
	ld l,c			;0ef3	69 	i
	ld l,d			;0ef4	6a 	j
	ld l,e			;0ef5	6b 	k
	ld l,h			;0ef6	6c 	l
	ld l,l			;0ef7	6d 	m
l0ef8h:
	ld l,(hl)			;0ef8	6e 	n
	ld l,a			;0ef9	6f 	o
	ld (hl),b			;0efa	70 	p
	ld (hl),c			;0efb	71 	q
	ld (hl),d			;0efc	72 	r
	ld (hl),e			;0efd	73 	s
	ld (hl),h			;0efe	74 	t
	ld (hl),l			;0eff	75 	u
	halt			;0f00	76 	v
	ld (hl),a			;0f01	77 	w
	ld a,b			;0f02	78 	x
	ld a,c			;0f03	79 	y
	ld a,d			;0f04	7a 	z
	ld e,e			;0f05	5b 	[
	ld e,h			;0f06	5c 	\
	ld e,l			;0f07	5d 	]
	dec l			;0f08	2d 	-
	ld a,a			;0f09	7f 	
l0f0ah:
	add hl,sp			;0f0a	39 	9
	nop			;0f0b	00 	.
	cp 0ffh		;0f0c	fe ff 	. .
	ld bc,0046h		;0f0e	01 46 00 	. F .
	nop			;0f11	00 	.
	nop			;0f12	00 	.
	ld bc,00a02h		;0f13	01 02 0a 	. . .
	rrca			;0f16	0f 	.
	rst 38h			;0f17	ff 	.
	ld (00007h),hl		;0f18	22 07 00 	" . .
	inc hl			;0f1b	23 	#
	rrca			;0f1c	0f 	.
l0f1dh:
	nop			;0f1d	00 	.
	dec b			;0f1e	05 	.
l0f1fh:
	inc bc			;0f1f	03 	.
	inc bc			;0f20	03 	.
	xor a			;0f21	af 	.
	djnz l0f9ah		;0f22	10 76 	. v
	nop			;0f24	00 	.
	cp 027h		;0f25	fe 27 	. '
	nop			;0f27	00 	.
	ld c,l			;0f28	4d 	M
	nop			;0f29	00 	.
	ld (bc),a			;0f2a	02 	.
	ld a,(bc)			;0f2b	0a 	.
	ld e,0e5h		;0f2c	1e e5 	. .
	xor (hl)			;0f2e	ae 	.
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
l0f4fh:
	cp 0dch		;0f4f	fe dc 	. .
	cp d			;0f51	ba 	.
	sbc a,b			;0f52	98 	.
l0f53h:
		;	Memory map
	db 0x89,0xab,0xcd,0xef
	db 0x48 	; . . H
	ld d,c			;0f58	51 	Q
	ld d,h			;0f59	54 	T
	ld c,l			;0f5a	4d 	M
	ld d,d			;0f5b	52 	R
	ld sp,03252h		;0f5c	31 52 32 	1 R 2
	ld d,d			;0f5f	52 	R
	inc sp			;0f60	33 	3
	ld d,d			;0f61	52 	R
	ld e,d			;0f62	5a 	Z
	ld sp,0005ah		;0f63	31 5a 00 	1 Z .
l0f66h:
	dec c			;0f66	0d 	.
	ld a,(bc)			;0f67	0a 	.
	ld c,(hl)			;0f68	4e 	N
	ld c,a			;0f69	4f 	O
	jr nz,l0fb0h		;0f6a	20 44 	  D
	ld c,c			;0f6c	49 	I
	ld d,e			;0f6d	53 	S
	ld c,e			;0f6e	4b 	K
	jr nz,l0fc0h		;0f6f	20 4f 	  O
	ld d,d			;0f71	52 	R
	jr nz,l0fb8h		;0f72	20 44 	  D
	ld c,c			;0f74	49 	I
	ld d,e			;0f75	53 	S
	ld c,e			;0f76	4b 	K
	jr nz,l0fc7h		;0f77	20 4e 	  N
	ld c,a			;0f79	4f 	O
	ld d,h			;0f7a	54 	T
	jr nz,l0fcfh		;0f7b	20 52 	  R
	ld b,l			;0f7d	45 	E
	ld b,c			;0f7e	41 	A
	ld b,h			;0f7f	44 	D
	ld b,c			;0f80	41 	A
	ld b,d			;0f81	42 	B
	ld c,h			;0f82	4c 	L
	push bc			;0f83	c5 	.
l0f84h:
	dec c			;0f84	0d 	.
	ld a,(bc)			;0f85	0a 	.
	ld c,(hl)			;0f86	4e 	N
	ld c,a			;0f87	4f 	O
	jr nz,$+85		;0f88	20 53 	  S
	ld e,c			;0f8a	59 	Y
	ld d,e			;0f8b	53 	S
	ld d,h			;0f8c	54 	T
	ld b,l			;0f8d	45 	E
	ld c,l			;0f8e	4d 	M
	jr nz,l0fe0h		;0f8f	20 4f 	  O
	ld c,(hl)			;0f91	4e 	N
	jr nz,l0fd8h		;0f92	20 44 	  D
	ld c,c			;0f94	49 	I
	ld d,e			;0f95	53 	S
	rrc l		;0f96	cb 0d 	. .
	ld a,(bc)			;0f98	0a 	.
	ld c,(hl)			;0f99	4e 	N
l0f9ah:
	ld c,a			;0f9a	4f 	O
	ld d,a			;0f9b	57 	W
	jr nz,l0fe7h		;0f9c	20 49 	  I
	ld c,(hl)			;0f9e	4e 	N
	jr nz,l0ff5h		;0f9f	20 54 	  T
	ld b,l			;0fa1	45 	E
	ld d,d			;0fa2	52 	R
	ld c,l			;0fa3	4d 	M
	ld c,c			;0fa4	49 	I
	ld c,(hl)			;0fa5	4e 	N
	ld b,c			;0fa6	41 	A
	ld c,h			;0fa7	4c 	L
	jr nz,l0ff7h		;0fa8	20 4d 	  M
	ld c,a			;0faa	4f 	O
	ld b,h			;0fab	44 	D
	ld b,l			;0fac	45 	E
	dec c			;0fad	0d 	.
	ld a,(bc)			;0fae	0a 	.
	adc a,d			;0faf	8a 	.
l0fb0h:
	nop			;0fb0	00 	.
	nop			;0fb1	00 	.
	nop			;0fb2	00 	.
	nop			;0fb3	00 	.
	nop			;0fb4	00 	.
	nop			;0fb5	00 	.
	nop			;0fb6	00 	.
	nop			;0fb7	00 	.
l0fb8h:
	nop			;0fb8	00 	.
	nop			;0fb9	00 	.
	nop			;0fba	00 	.
	nop			;0fbb	00 	.
	nop			;0fbc	00 	.
	nop			;0fbd	00 	.
	nop			;0fbe	00 	.
	nop			;0fbf	00 	.
l0fc0h:
	nop			;0fc0	00 	.
	nop			;0fc1	00 	.
	nop			;0fc2	00 	.
	nop			;0fc3	00 	.
	nop			;0fc4	00 	.
	nop			;0fc5	00 	.
	nop			;0fc6	00 	.
l0fc7h:
	nop			;0fc7	00 	.
	nop			;0fc8	00 	.
	nop			;0fc9	00 	.
	nop			;0fca	00 	.
	nop			;0fcb	00 	.
	nop			;0fcc	00 	.
	nop			;0fcd	00 	.
	nop			;0fce	00 	.
l0fcfh:
	nop			;0fcf	00 	.
l0fd0h:
	nop			;0fd0	00 	.
	ld bc,01f80h		;0fd1	01 80 1f 	. . .
	rra			;0fd4	1f 	.
	rra			;0fd5	1f 	.
	jr c,l0fd8h		;0fd6	38 00 	8 .
l0fd8h:
	nop			;0fd8	00 	.
	add a,b			;0fd9	80 	.
	nop			;0fda	00 	.
	ld b,b			;0fdb	40 	@
	ld bc,00200h		;0fdc	01 00 02 	. . .
	ld (de),a			;0fdf	12 	.
l0fe0h:
	nop			;0fe0	00 	.
	rra			;0fe1	1f 	.
	rra			;0fe2	1f 	.
	rra			;0fe3	1f 	.
	jr c,l0fe6h		;0fe4	38 00 	8 .
l0fe6h:
	nop			;0fe6	00 	.
l0fe7h:
	jr nz,l0fe9h		;0fe7	20 00 	  .
l0fe9h:
	ld b,b			;0fe9	40 	@
	nop			;0fea	00 	.
	djnz l0fedh		;0feb	10 00 	. .
l0fedh:
	nop			;0fed	00 	.
	nop			;0fee	00 	.
	nop			;0fef	00 	.
	nop			;0ff0	00 	.
	nop			;0ff1	00 	.
	nop			;0ff2	00 	.
	nop			;0ff3	00 	.
	defb 0edh;next byte illegal after ed		;0ff4	ed 	.
l0ff5h:
	inc c			;0ff5	0c 	.
	ld a,d			;0ff6	7a 	z
l0ff7h:
	dec c			;0ff7	0d 	.
	nop			;0ff8	00 	.
	nop			;0ff9	00 	.
	nop			;0ffa	00 	.
	nop			;0ffb	00 	.
	nop			;0ffc	00 	.
	nop			;0ffd	00 	.
	nop			;0ffe	00 	.
	nop			;0fff	00 	.
