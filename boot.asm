;--------------------------------------------------------------------
;
;   Boot loader for MITS (Burcon) CP/M 2.2 on a 88-DCCD Floppy
;
;   This loader is read in from track zero, sector 0-2 by the MITS DBL ROM
;
;   The loader reads all of CPM into the proper location in memory and then
;   jumps to the cold boot entry point.
;
;   Disassembled from data provided by W. Tom Sanderson
;
;   Source code regeneration by Mike Douglas, January 2013
;
;--------------------------------------------------------------------

;  CPM location equates

	 maclib	cpmsize		;bring in memory and bios size
ccpLen	 equ	0800h		;CPM 2.2 fixed
bdosLen	 equ	0e00h		;CPM 2.2 fixed
	 if	memSize
ccpBase	 equ	memSize*1024 - biosLen - bdosLen - ccpLen
	 else			;creating a relocatable image
ccpBase	 equ	0
	 endif
dosEnt	 equ	(ccpBase+ccpLen+6)	 ;entry address of BDOS
biosBase equ	(ccpBase+ccpLen+bdosLen) ;base address of this BIOS
bootSiz	 equ	3*128			 ;3 sectors for boot code
loadTk0	 equ	(ccpBase-bootSiz)	 ;wboot load address for track 0
loadTk1	 equ	(loadTk0 + 01000h)	 ;wboot load address for track 1

; MITS Disk Drive Controller Equates

drvSel	equ	8		;drive select port (out)
drvStat	equ	8		;drive status port (in)
drvCtl	equ	9		;drive control port(out)
drvSec	equ	9		;drive sector position (in)
drvData	equ	10		;drive read/write data

enwdMsk	equ	001h		;enter new write data bit mask
headMsk	equ	004h		;mask to get head load bit alone
moveMsk	equ	002h		;mask to get head move bit alone
trk0Msk	equ	040h		;mask to get track zero bit alone
dSelect	equ	0ffh		;deselects drive when written to drvSel
selMask	equ	008h		;"not used" bit is zero when selected

dcStepI	equ	001h		;drisve command - step in
dcStepO	equ	002h		;drive command - step out
dcLoad	equ	004h		;drive command - load head
dcUload	equ	008h		;drive command - unload head
dcWrite	equ	080h		;drive command - start write sequence

; disk information equates

numSect	equ	32		;number of sectors per track
altLen	equ	137		;length of Altair sector
secLen	equ	128		;length of CPM sector


; altBuf - Altair buffer contains the 137 bytes read straight from
;   the Altair drive. It is located 0100h bytes below the where
;   the CCP is loaded

altBuf	equ	ccpBase-0100h	;altair disk buffer

; Tracks 0-5 of Altair disks have this format

t0Trk	equ	0		;offset of track number
t0Data	equ	3		;offset of 128 byte data payload
t0Stop	equ	131		;offset of stop byte (0ffh)
t0CSum	equ	132		;offset of checksum


;-----------------------------------------------------------------------------
;  start of boot loader. Seek to track 0, load track 0, then load track 1,
;     then jump to the cold start BIOS entry.
;-----------------------------------------------------------------------------
	org	0
reloc	equ	$
start	lxi	sp,ccpBase	;stack grows down from lowest cpm address
	di
	xra	a		;select drive zero
	out	drvSel
selDrv	in	drvStat		;verify it was selected
	ani	selMask
	jnz	selDrv-reloc
	mvi	a,dcLoad	;load the head
	out	drvCtl
	jmp	chkTrk0-reloc	;go see if we're already on track 0

; seek0 - seek to track zero

seek0	in	drvStat		;loop until it's OK to move the head
	ani	moveMsk
	jnz	seek0-reloc
	mvi	a,dcStepO	;step out one track
	out	drvCtl
chkTrk0	in	drvStat		;loop until we get to track 0
	ani	trk0Msk
	jnz	seek0-reloc

; Load track 0 content into memory. Odd tracks are read 1st and stored
;    in every other 128 byte block. Then even tracks are read to fill
;    in the other 128 byte blocks.

	lxi	b,0100h		;b=sector 1, c=track 0 
	lxi	h,loadTk0	;track 0 load address
	call	loadTrk-reloc	;load all track 0 data

; Load track 1 content in the same manner.

chkTrk1	in	drvStat		;wait until OK to move the head
	ani	moveMsk
	jnz	chkTrk1-reloc
	mvi	a,dcStepI	;step in one track to track 1
	out	drvCtl
	lxi	b,0101h		;b=sector 1, c=track 1
	lxi	h,loadTk1	;track 1 load address
	call	loadTrk-reloc	;load off track 1 data

;  A flag is present in the BIOS that determines whether interrupts are
;    re-enabled or left disabled after executing disk code. That flag is
;    checked here. The flag in the BIOS is presently set to "re-enable
;    interrupts," so rather than trying to maintain a link to a byte in
;    the BIOS, we'll just re-enable interrupts.
;
;	lda	0EB59h		;flag to leave interrupts disabled in the BIOS
;	ani	010H		;the flag is true in the BIOS code
;	jz	biosBase	;so we don't bother with it here

	ei
	jmp	biosBase

;------------------------------------------------------------------------------
;  loadTrk - load a track into memory at it's final address
;------------------------------------------------------------------------------
loadTrk	push	b		;save sector and track number we're on
	push	h		;save destination address
	mov	a,h
	cpi	(ccpBase shr 8)  ;current address < cpp start addr?
	jc	nxtSec-reloc	;yes, move to next sector

; The following code was used to set an upper limit into which code would be
;    loaded. The first load puts the byte from location 0xff into A. The very
;    next instruction then loaded the value 0xff over that value. This value
;    is the MSB of the upper limit for storing code. The value 0xff does
;    nothing since it is the upper possible limit of memory anyway. A
;    different value could be put here if needed (e.g., 0xfc).
;
;	lda	0ffh		;upper limit stored in boot code at offset 0xff
;	mvi	a,0ffH		;over-ridden here. 0xff does nothing
;	cmp	h		;commented out so the upper limit is ignored
;	jc	nxtSec-reloc
		
	call	read-reloc	;read a sector
	jnz	start-reloc	;fatal read error, try again
nxtSec	pop	h		;get the destination pointer back
	lxi	d,0100h		;increment destination by 256 bytes
	dad	d
	pop	b		;get sector number back
	mov	a,b
	adi	2		;jump 2 setors each read
	mov	b,a
	cpi	numSect+1	;past 32 sectors?
	jc	loadTrk-reloc	;not yet, keep reading
	sui	numSect-1	;compute starting even sector number
	lxi	d,-0f80h	;compute load address for 1st even sector
	dad	d
	cpi	3		;done both odd and even sectors?
	mov	b,a
	jnz	loadTrk-reloc	;no, go to even sectors
	ret

;----------------------------------------------------------------------------
;  read - read a sector from the disk into altBuf, then move it to
;      the destination address after error checking. The sector number
;      is passed in B, the destination address is in HL.
;---------------------------------------------------------------------------
read	push	b		;save sector number
	push	h		;save address we're writing to next
	call	rdPSec-reloc	;read the sector specified in b into altBuf
	lxi	d,-6		;-6 bytes from the end is 0xff stop byte
	dad	d
	inr	m		;this should increment 0xff to zero
	pop	b		;bc = address we're writing to
	pop	d		;d = sector number, e = track number
	rnz			;exit if stop byte wasn't 0xff
	lxi	h,altBuf+t0Trk-reloc	;hl = track byte of altBuf
	mov	a,m		;get 1st byte with track number
	ani	07Fh		;get track number alone
	cmp	e		;verify track number is correct
	rnz			;exit if not right track
	inx	h		;move to 128 data payload at offset 3
	inx	h
	inx	h
	call	moveBuf		;move altBuf+t0Data to memory at (hl)
	lxi	h,altBuf+t0CSum-reloc
	cmp	m		;verify checksum matches
	ret

;----------------------------------------------------------------------------
; rdPSec - read physical sector. Read the sector specified by b into
;    altBuf. Physical sector length is altLen (137) bytes.
;----------------------------------------------------------------------------
rdPSec	dcr	b		;convert 1 indexed sector to 0 indexed
	call	secSync-reloc	;sync to start of sector specified in b
	mvi	c,altLen	;c = length of Altair sector (137 bytes)
	lxi	h,altBuf-reloc	;point hl to altBuf
rdLoop	in	drvStat		;get drive status byte
	ora	a		;wait for NRDA flag true (zero)
	jm	rdLoop-reloc
	in	drvData		;read the byte 
	mov	m,a		;store in the read buffer
	inx	h		;increment buffer pointer
	dcr	c		;decrement characters remaining counter
	jz	rdDone-reloc	;count is zero - read is done
	dcr	c		;decrement count for 2nd byte about to be read
	nop			;timing
	in	drvData		;get the 2nd byte
	mov	m,a		;store in the read buffer
	inx	h		;increment buffer pointer
	jnz	rdLoop-reloc	;loop until all bytes read
rdDone	xra	a		;return status of zero = good read
	ret

; secSync - sync to start of sector specified in b.

secSync	call	ldHead-reloc		;load the drive head
wtSecTr	in	drvSec
	rar
	jc	wtSecTr-reloc
	ani	01fh		;get sector number alone
	cmp	b		;match sector number we're looking for
	jnz	wtSecTr-reloc
	ret

;----------------------------------------------------------------------------
; moveBuf - move sector buffer (128 bytes) from (hl) to (bc). Compute
;   checksum on all bytes and return in a.
;----------------------------------------------------------------------------
moveBuf	mvi	d,0		;init checksum to zero
	mvi	e,secLen	;128 byte data sector length
movLoop	mov	a,m		;move from (hl) to (bc)
	stax	b
	add	d		;add moved byte to checksum
	mov	d,a		;checksum saved in d
	inx	h		;increment both pointers
	inx	b
	dcr	e		;loop until all characters moved
	jnz	movLoop-reloc
	ret

;----------------------------------------------------------------------------
;  ldHead - If head already loaded, return. Otherwise, issue head load 
;     command.
;----------------------------------------------------------------------------
ldHead	in	drvStat
	ani	headMsk		;get head loaded bit alone
	rz			;zero = true, head already loaded
	mvi	a,dcLoad	;issue the load head command
	out	drvCtl
	ret

        rept    (start +0180h - $)	;boot is exactly 180h length
        db      0
        endm

	end

