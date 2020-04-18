;--------------------------------------------------------------------
;
;	CP/M 2.2 BIOS for MITS Altair 8800x and 88-DCCD Floppy Drive
;
;	Disassembled from a memory dump provided by W. Tom Sanderson
;
;	Source code regeneration by Mike Douglas, January 2013
;
;--------------------------------------------------------------------
	
;  CPM location equates

	 maclib	cpmsize		;bring in memory and bios size
ccpLen	 equ	0800h		;CPM 2.2 fixed
bdosLen	 equ	0e00h		;CPM 2.2 fixed
	 if	memSize
ccpBase	 equ	memSize*1024 - biosLen - bdosLen - ccpLen
bdosEnt	 equ	(ccpBase+ccpLen+6)	 ;entry address of BDOS
biosBase equ	(ccpBase+ccpLen+bdosLen) ;base address of this BIOS
	 org	biosBase
	 else
biosBase equ	(ccpLen+bdosLen) ;base address of this BIOS
	 org	biosBase
relBias	 equ	$-biosBase	 ;get relocation bias
ccpBase	 equ	$-bdosLen-ccpLen
bdosEnt	 equ	$-bdosLen	 ;entry address of BDOS
	 endif
bootSiz	 equ	3*128			 ;3 sectors for boot code
loadTk0	 equ	(ccpBase-bootSiz)	 ;wboot load address for track 0
loadTk1	 equ	(loadTk0 + 01000h)	 ;wboot load address for track 1

; CPM page zero equates

wbootv	equ	000h		;warm boot vector location
bdosv	equ	005h		;bdos entry vector location
cdisk	equ	004h		;CPM current disk
defDma	equ	080h		;default dma address

; 2SIO Serial Board Equates

sio1Ctl	equ	010h		;1st port on 2SIO board - control register
sio1Dat	equ	011h		;1st port on 2SIO board - data register
sio2Ctl	equ	012h		;2nd port on 2SIO board - control register
sio2Dat	equ	013h		;2nd port on 2SIO board - data register
sioRdrf	equ	001h		;read data register full flag
sioTdre	equ	002h		;transmit data register empty flag

; LPC Line Printer Controller Equates

lpcCtl	equ	002h		;line printer control port
lpcData	equ	003h		;line printer data port
lpcBusy	equ	002h		;mask to test printer busy flag

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
selMask	equ	008h		;"not used" bit is zero when drive selected

dcStepI	equ	001h		;drive command - step in
dcStepO	equ	002h		;drive command - step out
dcLoad	equ	004h		;drive command - load head
dcUload	equ	008h		;drive command - unload head
dcWrite	equ	080h		;drive command - start write sequence

; disk information equates

numDisk	equ	4		;up to four drives in use
numTrk	equ	77		;number of tracks
numSect	equ	32		;number of sectors per track
altLen	equ	137		;length of Altair sector
secLen	equ	128		;length of CPM sector
dsm0	equ	149		;max block number (150 blocks of 2048 bytes)
drm0	equ	63		;max directory entry number (64 entries) 
cks	equ	(drm0+1)/4	;directory check space
maxTry	equ	5		;maximum number of disk I/O tries
unTrack	equ	07fh		;unknown track number

; misc equates

cr	equ	13		;ascii for carriage return
lf	equ	10		;ascii for line feed

;-----------------------------------------------------------------------------
;
;  BIOS Entry Jump Table
;
;-----------------------------------------------------------------------------
	jmp	boot		;cold start
wboote	jmp	wboot		;warm start
	jmp	const		;console status
	jmp	conin		;console character in 
	jmp	conout		;console character out 
	jmp	list		;list character out 
	jmp	punch		;punch character out 
	jmp	reader		;reader character out
	jmp	home		;move head to home position
	jmp	seldsk		;select disk
	jmp	settrk		;set track number
	jmp	setsec		;set sector number
	jmp	setdma		;set dma address
	jmp	read		;read disk
	jmp	write		;write disk
	jmp	listst		;return list status
	jmp	sectran		;sector translate

;  CPM Welcome Message

cpmStr	db	cr, lf, lf
cpmSize db	'64K CP/M', cr, lf
	db	'Version 2.2mits (07/28/80)', cr, lf
	db	'Copyright 1980 by Burcon Inc.', cr, lf, 0

dbugStk	equ	$		;debug trace stack grows down from here

;----------------------------------------------------------------------------
; boot - Cold boot BIOS entry. CPM is already loaded in memory. Hand
;    control to the CCP.
;----------------------------------------------------------------------------
boot	xra	a
	sta	cdisk		;current CPM disk is drive zero
	call	serinit		;initialize both serial ports on a 2SIO board

; form the two digit CPM size in ascii (e.g., "60") for "60K")

	lxi	h,biosEnd	;cpm size marker
	mov	a,h		;convert MSB of CPM marker to 1K count
	ani	0fch		;get bits for 1K inrcrements
	jz	welcome		;0 will assume 64K
	rar
	rar
	mvi	l,'0'		;start with ascii "0" for the 10's digit

tenCnt	sui	10		;divide by 10 to get the 10's digit
	jm	tenDone
	inr	l		;count the number of 10's in ascii
	jmp	tenCnt

tenDone adi	03ah		;compute final 1's digit in ascii
	mov	h,a		;h = 1's digit in ascii
	shld	cpmSize		;store two digit ascii size in welcome msg

;  Output the CPM welcome message

welcome	lxi	h,cpmStr	;hl = address of cpm welcome message
welOut	mov	c,m
	call	conout
	inx	h
	mov	a,m
	ora	a
	jnz	welOut		;string ends with terminating null
	jmp	entCpm

;----------------------------------------------------------------
; wboot - Warm boot BIOS entry. Reload CPM from disk up to, but
;    not including the BIOS. Re-enter CPM after loading.
;----------------------------------------------------------------
wboot	lxi	sp,0100h	;init stack pointer
	xra	a
	sta	diskNum		;disk zero is boot disk
	call	selTrk0		;select disk and seek to track 0
	lda	flags		;turn the raw disk I/O flag off
	ori	fRawIO
	xri	fRawIO	
	sta	flags
	mvi	c,0		;select disk zero
	call	seldsk
	mvi	c,0		;select track zero
	call	settrk
	lxi	h,loadTk0	;track 0 load address  
	mvi	a,1		;start on sector 1 (1-32, not 0-31)
	call	loadTrk
	mvi	c,1		;now do track 1
	call	settrk
	lxi	h,loadTk1	;track 1 load address
	mvi	a,1		;start on sector 1 (1-32, not 0-31)
	call	loadTrk
	jmp	entCpm

; loadTrk - load one track into memory. Read odd sectors into every other 
;    128 bytes of memory until the BIOS base address is reached or all
;    32 sectors in the track have been read. Then read even sectors into
;    the opposite 128 byte sections of memory until the BIOS base address
;    is reached or all 32 sectors in the track have been read.

loadTrk	sta	secNum		;save the sector number to we're on
	shld	dmaAddr		;save the destination address
	mov	a,h
	cpi	(biosBase shr 8)  ;current address >= bios start address?
	jnc	nxtSec		;yes, skip read (don't overwrite bios)
	cpi	(ccpBase shr 8)	;current address < ccp start address?
	jc	nxtSec		;yes, skip read (not to valid data yet)
	call	read		;read a sector
	jnz	wboot		;fatal read error
nxtSec	lhld	dmaAddr		;hl = destination address
	lxi	d,0100h		;increment destination address by 256 bytes
	dad	d
	lda	secNum		;a = sector number
	adi	2		;jump 2 sectors each read
	cpi	numSect+1	;past the last sector in the track?
	jc	loadTrk		;not yet, keep reading
	sui	numSect-1	;compute starting even sector number
	lxi	d,-0f80h	;compute load address for 1st even sector
	dad	d
	cpi	3		;done both odd and even sectors in a track?
	jnz	loadTrk		;no, go do even sectors
	ret

; entCpm - enter CPM. Set page zero variables, enter CPM with or without
;   command line based on 

entCpm	lxi	b,defDma	;set the default dma address (080h)
	call	setdma
	mvi	a,0c3h		;8080 "jump" opcode
	sta	wbootv		;store in 1st byte of warm boot vecctor
	sta	bdosv		;and 1st byte of bdos entry vecctor
	lxi	h,wboote	;get the warm boot address
	shld	wbootv+1	;and put it after the jump
	lxi	h,bdosEnt	;BDOS entry address
	shld	bdosv+1		;put it after the jump opcode
	lda	cdisk		;get current disk number
	mov	c,a		;pass it to CPM in register c
	lxi	h,cldDone	;get the "cold start done" flag
	mov	a,m
	ora	a
	mvi	m,1		;set the flag true - cold start has been done
	lda	flags		;check the proper "process command line" flag
	jz	coldSt		;jump if this is cold start
	ani	fWrmCmd		;warm start - run default command line?
	jmp	cmdChk
coldSt	ani	fCldCmd		;cold start - run default command line?
cmdChk	jz	ccpBase+3	;no, enter ccp and clear cmd line
	jmp	ccpBase		;yes, enter ccp with possible cmd line

;----------------------------------------------------------------------------
; seldsk - Select Disk BIOS entry. C contains the disk number to select.
;    Validate the disk number and return a pointer to the disk parameter
;    header in HL. Zero is returned in HL for invalid drive number. The
;    selected disk number is stored in diskNum. No actual drive activity 
;    takes place.
;----------------------------------------------------------------------------
seldsk	lxi	h,0		;hl of zero is error flag
	mov	a,c		;a = drive number
	cpi	numDisk		;verify drive number less than number of disks
	rnc			;invalid drive number, return with hl=0=error
	mov	a,c
	sta	diskNum		;save the selected disk number
	rlc			;compute disk parameter header address
	rlc			;(16*drvNum) + dpHead
	rlc
	rlc
	lxi	h,dpHead  	;start of disk parameter headers.
	mov	e,a
	mvi	d,0
	dad	d		;hl points to the DPH for the passed drive
	ret

;----------------------------------------------------------------------------
; home - Home BIOS entry. Move to track zero position.
;----------------------------------------------------------------------------
home	jmp	selTrk0		;select disk and seek to track 0

;----------------------------------------------------------------------------
; settrk - Set Track BIOS entry. C contains the desired track number.
;    The track number is saved in trkNum for later use. No actual
;    drive activity takes place.
;----------------------------------------------------------------------------
settrk	mov	a,c
	sta	trkNum
	ret

;----------------------------------------------------------------------------
; setsec - Set Sector BIOS entry. C contains the desired sector number.
;    The sector number has already been translated through the skew table.
;    The sector number is saved in secNum for later use. No actual
;    drive activity takes place.
;----------------------------------------------------------------------------
setsec	mov	a,c
	sta	secNum
	ret

;----------------------------------------------------------------------------
; setdma - Set DMA BIOS entry. BC contains the address for reading or
;    writing sector data for subsequent I/O operations. The address is
;    stored in dmaAddr.
;----------------------------------------------------------------------------
setdma	mov	h,b
	mov	l,c
	shld	dmaAddr
	ret

;----------------------------------------------------------------------------
; read - Read sector BIOS entry. Read one sector using the trkNum, secNum
;    and dmaAddr specified for diskNum. Returns 0 in A if successful, 1 in A
;    if a non-recoverable error has occured.
;----------------------------------------------------------------------------
read	call	swapStk		;swap to debug trace stack
	mvi	a,1		;set the "verify track number" flag to true
	call	rTrkSec		;retrieve track in c, physical sector in b
	di
	call	readSec		;read the sector
	jmp	exitDio		;disk i/o exit routine

;----------------------------------------------------------------------------
; write - Write sector BIOS entry. Write one sector using the trkNum, secNum
;    and dmaAddr specified for diskNum. Returns 0 in A if successful, 1 in A
;    if a non-recoverable error has occured.
;----------------------------------------------------------------------------
write	call	swapStk
	xra	a		;set the "verify track number" flag to false
	call	rTrkSec		;retrieve track in c, physical sector in b
	di
	call	wrtSec		;write sector
	jnz	exitDio		;write failed, exit
	lda	flags
	ani	fWrtVfy		;are we verifying writes?
	jz	exitDio		;no, exit
	mvi	a,1		;set the "verify track number" flag to true
	call	rTrkSec		;retrieve track in c, physical sector in b
	lxi	h,altBuf	; (this isn't actually used by readSec)
	call	readSec		;read the sector just written

;  exitDio - exit disk i/o. Restore interrupts if enable interrupt flag is
;     set.  

exitDio	push	psw
	lda	flags
	ani	fEnaInt		;should we re-enable interrupts?
	jz	noInts		;no
	ei
noInts	pop	psw
	mvi	a,0		;return error code of zero (success)
	rz
	lxi	h,failCnt	;increment total fail count
	inr	m
	mvi	a,1		;return fail code of one
	ora	a
	ret

;----------------------------------------------------------------------------
; sectran - Sector translation BIOS entry. Convert logical sector number in
;    BC to physical sector number in HL using the skew table passed in DE.
;----------------------------------------------------------------------------
sectran	xchg			;put table address (DE) into HL
	dad	b		;offset by logical sector number
	mov	l,m		;get physical sector number in L
	mvi	h,0		;for H to zero
	ret

;  rTrkSec - return track in c, physical sector in b

rTrkSec	sta	trkVrfy		;save the passed track verify flag
	lhld	trkNum		;L = track number, H = sector number
	mov	c,l
	dcr	h		;convert 1-32 to 0-31 for altair hardware
	mov	b,h
	ret

;----------------------------------------------------------------------------
; conin - Console input character BIOS entry point. Reads a character
;    into A from the console port (2SIO board, ACIA #1)
;----------------------------------------------------------------------------
conin	call	uldHead		;if waiting for input, assume disk I/O is done
	jmp	sio1In		;read character from sio acia #1

;--------------------------------------------------------------------------
; swapStk - swap in debug stack for real stack. When the caller of this
;   routine does a ret, the return jumps to "resStk" below which restores
;   the normal stack.
;--------------------------------------------------------------------------
swapStk	pop	d		;de = return address. Return address popped
	lxi	h,0
	dad	sp		;hl = stack pointer after return addr popped
	lxi	sp,dbugStk	;use dbugStk as the new stack area
	push	h		;save original stack pointer
	lxi	h,resStk	;next "ret" will go to resStk, below
	push	h
	xchg			;put original return address in HL
	pchl			;jump to the "return" address

resStk	pop	h		;HL = original stack pointer
	sphl			;put original stack pointer back
	ret			;return to caller of swapStk caller

;-----------------------------------------------------------------------------
; disk control data
;-----------------------------------------------------------------------------
diskNum	ds	1		;current disk number
trkNum	ds	1		;track num (sector num MUST follow in memory)
secNum	ds	1		;sector number for disk operations
dmaAddr	ds	2		;dma address for disk operations
trkVrfy	ds	1		;verify track numbers if <> 0
selNum	ds	1		;disk number currently selected on controller
	db	0
rtryCnt	ds	1		;retry counter
cldDone	db	0		;true after cold start has completed

; trkTbl - track information for each drive. Init to unkown track number.

trkTbl	db	unTrack, unTrack, unTrack, unTrack

;-----------------------------------------------------------------------------
; readSec - read sector. Selects the drive in diskNum, seeks to the track
;    in C and then reads the sector specified in B into the buffer
;    pointed to by dmaAddr.
;----------------------------------------------------------------------------
readSec	call	selTrk		;select the drive, seek to the proper track
	rnz			;exit if error
reReadP	push	b		;save sector number
reRead	call	rtryChk		;decrement and check retry counter
	pop	b
	rnz			;exit if no more retries
	push	b
	mov	a,b		;a = sector number
	call	altSkew		;do 17 sector skew like Altair Disk Basic does
	lxi	h,altBuf	;hl points to altair sector buffer
	call	rdPSec		;read physical sector
	lda	flags		;raw I/O or normal move to (dmaAddr)?
	ani	fRawIO
	jnz	rdExit		;raw I/O, leave data in altBuf and exit
	lda	trkNum		;process data differently depending on track#
	cpi	6		;tracks 0-5 processed directly below
	jnc	rTrk676		;jump for tracks 6-76

; validate and move data for sectors in tracks 0 - 5

	lxi	h,altBuf+t0Stop	;should have 0ffh at byte 131 in altBuf
	inr	m
	jnz	reRead		;wasn't 255, re-try the sector read
	lxi	h,altBuf+t0Trk	;track number at byte 0 in altBuf
	mov	a,m
	ani	07Fh		;get track number alone
	pop	b
	cmp	c		;track number in c match track # in data?
	jnz	rdTrkEr		;no, have track number error
	push	b
	lhld	dmaAddr		;hl = destination address for the data
	mov	b,h		;put hl into bc
	mov	c,l
	lxi	h,altBuf+t0Data	;data starts at byte 3 in altBuf
	call	moveBuf		;move cpm sector to (dmaAddr), return checksum
	lxi	h,altBuf+t0CSum	;sector checksum is in altBuf + 132
	cmp	m
	jnz	reRead		;checksum fail, re-try the sector read
	pop	b
	xra	a		;return success code
	ret

;  rTrk676 - validate and move data for sectors in tracks 6-76

rTrk676	dcx	h		;move back to last byte read (offset 136)
	mov	a,m		
	ora	a		;verify it is zero
	jnz	reRead		;not zero, re-try the sector read
	dcx	h		;move back to offset 135
	inr	m		;should have 0ffh here
	jnz	reRead		;0ffh not present, re-try the sector read
	lxi	h,altBuf+t6Trk	;verify 1st byte of altBuf matches track #
	mov	a,m
	ani	07Fh		;get track number alone
	pop	b
	cmp	c		;track number in c match track # in data?
	jnz	rdTrkEr		;no, have track number error
	inx	h		;move to offset 1, should have sector num here
	mov	a,m
	cmp	b		;verify it matches requested sector number
	jnz	reReadP		;sector match fail, retry the sector read
	push	b
	lhld	dmaAddr		;hl = destination address for the data
	mov	b,h		;put hl into bc
	mov	c,l
	lxi	h,altBuf+t6Data	;data starts at byte 7 in altbuf
	call	moveBuf		;move cpm sector to (dmaAddr), return checksum
	lxi	h,altBuf+6	;add bytes 2,3,5 and 6 to checksum
	mov	b,m		;b = byte 6
	dcx	h
	mov	c,m		;c = byte 5
	dcx	h
	mov	d,m		;d = byte 4 (checksum byte)
	dcx	h
	mov	e,m		;e = byte 3
	dcx	h		;m = byte 2
	add	e		;add bytes 3, 6, 5 and 2 (not 4) to checksum
	add	b
	add	c
	add	m
	cmp	d		;compare to checksum
	jnz	reRead		;checksum fail, re-try the sector read
	pop	b
	xra	a		;otherwise, return success code
	ret

; rdTrkEr - Track number error during the read operation

rdTrkEr	xra	a
	call	reTkSec		;retry the track and sector seek
	jmp	reReadP		;retry the sector read (push B entry)

;  rdExit - exit read (raw) where data is left in altBuf

rdExit	pop	b
	xra	a
	ret

; rdPSec - read physical sector. Read the physical Altair sector (0-31)
;    specified by e into the buffer specified by HL. Physical sector
;    length is altLen (137) bytes.

rdPSec	call	secSync		;sync to start of sector specified in e
	mvi	c,altLen	;c = length of Altair sector (137 bytes)
rdLoop	in	drvStat		;get drive status byte
	ora	a		;wait for NRDA flag true (zero)
	jm	rdLoop
	in	drvData		;read the byte 
	mov	m,a		;store in the read buffer
	inx	h		;increment buffer pointer
	dcr	c		;decrement characters remaining counter
	jz	rdDone		;count is zero - read is done
	dcr	c		;decrement count for 2nd byte about to be read
	nop			;timing
	in	drvData		;get the 2nd byte
	mov	m,a		;store in the read buffer
	inx	h		;increment buffer pointer
	jnz	rdLoop		;loop until all bytes read
rdDone	xra	a		;return status of zero = good read
	ret

; secSync - sync to start of sector specified in e. The sector number in e
;    is an Altair physical sector number (0-31).

secSync	call	ldHead		;load the drive head
wtSecTr	in	drvSec		;read sector position register
	rar			;wait for sector true flag (zero = true)
	jc	wtSecTr
	ani	01fh		;get sector number alone
	cmp	e		;match sector num we're looking for?
	jnz	wtSecTr
	ret

;------------------------------------------------------------------------------
; wrtSec - Write a sector. Selects the drive in diskNum, seeks to the
;    track in C and then writes the sector specified in B from the
;    buffer pointed to by dmaAddr.
;-----------------------------------------------------------------------------
wrtSec	call	selTrk		;select the drive, seek to the proper track
	rnz			;return if it failed
	lda	flags		;see if raw I/O flag is set
	ani	fRawIO
	jnz	setHCS		;raw I/O, write altBuf, go set head current
	lda	trkNum		;process data differently depending on track #
	cpi	6		;tracks 0-5 processed directly below
	jnc	wTrk676		;jump to process tracks 6-76

;  Sector write for tracks 0-5

	push	b		;save sector number
	mov	a,c		;a = track number
	lxi	b,altBuf	;bc points to altair sector buffer
	stax	b		;put track number at offset zero
	xra	a		;put 0100h (16 bit) at offset 1,2
	inx	b
	stax	b
	inr	a
	inx	b
	stax	b
	inx	b		;bc = cpm data in altBuf at offset 3
	lhld	dmaAddr		;hl = location from which to read data
	call	moveBuf		;move cpm sector from (dmaAddr) to altbuf
	mvi	a,0ffh		;offset 131 is stop byte (0ffh)
	stax	b
	inx	b		;offset 132 is checksum
	mov	a,d		;a = checksum
	stax	b		;store it at offset 132
	pop	b
	jmp	setHCS		;go set head current setting

;  wTrk676 - write sector for tracks 6-76

wTrk676	push	b		;save sector number
	lxi	b,altBuf+t6Data	;bc = cpm data in altBuf at offset 7
	lhld	dmaAddr		;hl = location from which to read data
	call	moveBuf		;move cpm sector from (dmaAddr) to altbuf
	mvi	a,0ffh		;offset 135 is stop byte (0ffh)
	stax	b
	inx	b		;offset 136 is unused (store zero)
	xra	a
	stax	b
	mov	a,d		;a = checksum
	lhld	altBuf+2	;add bytes at offset 2 and 3 to checksum
	add	h
	add	l
	lhld	altBuf+5	;add bytes at offset 5 and 6 to checksum
	add	h
	add	l
	sta	altBuf+t6CSum	;store final checksum at offset 4
	pop	b
	lxi	h,altBuf	;hl = pointer to altair sector buffer
	mov	m,c		;store track number at offset 0
	inx	h
	mov	m,b		;store sector number at offset 1

; setHCS - set the head current reduction (bit 6) for tracks 43-76.
;   Also set the write bit (bit 7)

setHCS	mov	a,c		;a = track number
	adi	(-43 and 0ffh)	;add -43 (1st track for HCS bit = 1)
	mvi	a,0		;set a=0 without affecting carry
	rar			;080h if track >= 43
	stc
	rar			;0c0 if track >= 43, else 080h
	mov	d,a		;d is the control command for the drive
	mov	a,b		;a = sector number
	call	altSkew		;do sector skew like Altair Disk Basic

; wait for sector true and the right sector number

wtWrSec	in	drvSec		;read sector position register
	rar			;wait for sector true (0=true)
	jc	wtWrSec
	ani	01fh		;get sector number alone
	cmp	e		;at the right sector yet?
	jnz	wtWrSec		;no, keep looking
	mov	a,d		;control command formed in d above (setHCS)
	out	drvCtl		;issue write command
	lxi	h,altBuf	;hl points to altair sector buffer
	lxi	b,0100h+altLen	;c=137 bytes to write, b=1 byte of 0's at end
	mvi	d,enwdMsk	;Enter New Write Data flag mask
	mvi	a,080h		;1st byte of sector is sync byte
	ora	m		;sync byte must have msb set
	mov	e,a		;e = next byte to write
	inx	h

; wrSec - write physical sector loop

wrSec	in	drvStat		;read drive status register
	ana	d		;write flag (ENWD) asserted (zero)?
	jnz	wrSec		;no, keep waiting
	add	e		;put byte to write into accumulator
	out	drvData		;write the byte
	mov	a,m		;a = next byte to write
	inx	h		;increment source buffer pointer
	mov	e,m		;e = byte to write next time through this loop
	inx	h		;increment source buffer pointer
	dcr	c		;decrement chars remaining (byte just written)
	jz	wrDone		;zero - sector data written
	dcr	c		;dec chars remaining (byte about to write)
	out	drvData		;write 2nd byte
	jnz	wrSec		;loop if count <> 0

; wrDone - write is done. Now write # of zeros specified in b (just 1)

wrDone	in	drvStat		;wait for another write flag
	ana	d
	jnz	wrDone
	out	drvData		;write zero b times
	dcr	b
	jnz	wrDone
	xra	a		;return success status
	ret

;------------------------------------------------------------------------------
; moveBuf - move sector buffer (128 bytes) from (hl) to (bc). Compute
;   checksum on all bytes and return in a.
;------------------------------------------------------------------------------
moveBuf	mvi	d,0		;d = checksum
	mvi	e,secLen	;e = buffer length (128 bytes)
movLoop	mov	a,m		;move from (hl) to (bc)
	stax	b
	add	d		;add byte to checksum
	mov	d,a
	inx	b		;increment both pointers
	inx	h
	dcr	e		;decrement character count
	jnz	movLoop		;loop until count = 0
	ret

;------------------------------------------------------------------------------
; altSkew - Do Altair sector skew like disk basic. For sectors greater than 6,
;    physical = (logical * 17) mod 32. Returns physical sector number in e.
;    This is done on top of the secTran skew table mechanism of CPM. The math
;    works out such that this call to altSkew does almost nothing.
;------------------------------------------------------------------------------
altSkew	mov	e,a		;e = input sector number
	lda	trkNum		;see if track number >= 6
	cpi	6
	rc			;track < 6, exit
	mov	a,e		;multiply by 17
	add	a
	add	a
	add	a
	add	a
	add	e
	ani	01Fh		;keep lower 5 bits (0-31)
	mov	e,a
	ret

;------------------------------------------------------------------------------
;  selTrk - select the drive, go to the proper track (specified in diskNum 
;     and C)
;------------------------------------------------------------------------------
selTrk	mvi	a,maxTry	;set retry count (5 tries)
	sta	rtryCnt
	call	selDrv		;returns with HL=trkTable entry for this drive
	rnz
	mov	a,m		;a = track this drive is presently on
	cpi	unTrack		;if unknown track number, seek to track zero
reTkSec	cz	ldSeek0		;if 7f, load head and seek to track 0
	mov	a,m		;a = track number drive is presently on
	mov	m,c		;put destination track num in the track table
	mov	e,a		;e = track number drive is presently on
	call	ldHead		;load the drive head
	mov	a,e		;a = current track, c = desired track
	sub	c		;a = current - desired
	rz			;difference is zero - on proper track already
	mvi	d,dcStepI	;step in to higher tracks if desired > current
	jc	mulStep
	mvi	d,dcStepO	;step out to lower tracks if desired < current
	cma			;make it a negative value (1's complement + 1)
	inr	a

; mulStep - issue multiple track steps in or out (controller step command
;    in d). The number of tracks to step is passed as -steps. E.g., 10
;    steps passed as -10.

mulStep	mov	e,a		;e = -tracks left to step
wMoveOk	in	drvStat		;get drive status register
	ani	moveMsk		;wait until it's OK to move the head
	jnz	wMoveOk
	mov	a,d		;issue step in or out per command in d
	out	drvCtl	
	mov	a,e		;e has -tracks to step, counting up to zero
	inr	a
	jnz	mulStep		;loop until we've stepped to the desired track
	call	rtryChk		;decrement and check retry counter
	jz	stpDone		;retry count OK
	call	selTrk0		;max tries reached. Return error.
	jmp	retErr

stpDone	lda	trkVrfy		;track number verification flag set?
	ora	a
	jnz	vrfyTrk		;yes, verify track on disk = computed track
	lda	flags		;see if the force track verify flag is set
	ani	fTrkVfy		;if so, verify the track data anyway
	rz

;  vrfyTrk - Verify we're on the right track (passed in c) by reading
;     the track number that is stored in the sync byte at the start
;     of every sector. We don't care which sector we read.

vrfyTrk	in	drvSec		;wait for sector true (any sector)
	rar
	jc	vrfyTrk
wtVrfy	in	drvStat		;wait for new read data available flag
	ora	a
	jm	wtVrfy		;wait for NRDA flag
	in	drvData		;get sync byte (contains track number)
	ani	07Fh		;get the track number alone
	cmp	c
	rz			;we're on the right track number
	xra	a
	jmp	reTkSec		;otherwise, retry track and sector seek

;-----------------------------------------------------------------------------
;  selDrv - select the drive specified in diskNum
;-----------------------------------------------------------------------------
selDrv	lda	diskNum		;diskNum is the disk that CPM wants to use
	mov	e,a
	lda	selNum		;disk currently selected on the controller
	cmp	e		;same drive as already selected?
	mov	a,e
	sta	selNum		;set selected drive = diskNum
	jnz	selNew		;not the same drive, select the new drive
	in	drvStat		;make sure the drive is still selected
	ani	selMask
	jz	rTrkTbl		;drive selected, retrieve the track table ptr

;  selNew - select a new drive, deselect the current, then select the new

selNew	mvi	a,dSelect	;deselect the attached drive
	out	drvSel
	lda	selNum		;drive number to select
	out	drvSel
	in	drvStat		;loop until drive is selected
	ani	selMask
	jnz	selNew

; rTrkTbl - return track table pointer. Also validates the drive number 
;    passed in e. Returns with HL pointing to the trkTbl entry for drive in e.

rTrkTbl	mvi	a,4		;drive number OK? (should be 3 here?)
	cmp	e		;e has drive number
	jc	retErr		;error if drive number > 4 (should 3 be used?)
	lxi	h,trkTbl   	;index by drive number (e) into the track table
	mvi	d,0		;de = 16 bit drive number
	dad	d
	ora	m		;test for msb set in track number
	jm	retErr		;track number invalid if msbit is set
	xra	a		;else, return zero
	ret

;------------------------------------------------------------------------------
;  ldHead - If head already loaded, return. Otherwise, issue the head load
;	command and wait for it to load.
;------------------------------------------------------------------------------
ldHead	in	drvStat		;get drive status
	ani	headMsk		;get head loaded bit alone
	rz			;zero = true, head is loaded
	mvi	a,dcLoad	;issue the load head command
	out	drvCtl
wtHead	in	drvStat		;get drive status
	ani	headMsk		;wait for head loaded to be true
	jnz	wtHead
	ret

;------------------------------------------------------------------------------
;  uldHead - Issued the unload head command
;------------------------------------------------------------------------------
uldHead	mvi	a,dcUload	;issue head unload command
	out	drvCtl
	ret

;-----------------------------------------------------------------------------
; selTrk0 - Select drive in diskNum, load head and seek to track 0
;   ldSeek0 - Load head and seek to track 0
;-----------------------------------------------------------------------------
selTrk0	call	selDrv		;select drive in diskNum
ldSeek0	call	ldHead		;load the drive head
seek0	in	drvStat		;get drive status register
	ani	moveMsk		;loop until OK to move the head
	jnz	seek0		
	mvi	a,dcStepO	;issue step out command
	out	drvCtl
	in	drvStat
	ani	trk0Msk		;loop until we get to track zero
	jnz	seek0
	lda	selNum		;selNum is diskNum once drive is selected
	mov	e,a
	call	rTrkTbl		;HL = track table pointer for this drive
	mvi	m,0		;set track number for this drive to zero
	ret

;------------------------------------------------------------------------------
; rtryChk - retry counter check. Decrement retry counter. Returns zero if
;     more tries left, non-zero if retry counter reaches zero.
;------------------------------------------------------------------------------
rtryChk	lda	rtryCnt
	dcr	a
	sta	rtryCnt
	jz	retErr
	xra	a
	ret

; retErr - Return error code with 1 in accumulator and non-zero status flag

retErr 	mvi	a,1
	ora	a
	ret

;------------------------------------------------------------------------------
; altBuf - Altair buffer contains the 137 bytes read straight from
;   the Altair drive. This BIOS assumes the disk is laid out in a 
;   manner similar to the Altair Disk Basic format. Sectors in tracks 0-5
;   have a different layout than sectors in tracks 6-76.
;------------------------------------------------------------------------------
altBuf	ds	137		;altair disk buffer

; Tracks 0-5

t0Trk	equ	0		;offset of track number
t0Data	equ	3		;offset of 128 byte data payload
t0Stop	equ	131		;offset of stop byte (0ffh)
t0CSum	equ	132		;offset of checksum

; Tracks 6-76

t6Trk	equ	0		;offset of track number
t6Sec	equ	1		;offset of sector number
t6CSum	equ	4		;offset of checksum
t6Data	equ	7		;offset of 128 byte data payload
t6Stop	equ	135		;offset of stop byte (0ffh)
t6Zero	equ	136		;offset of unused, but checked for zero.

	ds	5		;not used

;-----------------------------------------------------------------------------
; dpHead - disk parameter header for each drive
;-----------------------------------------------------------------------------
dpHead	dw	xlate,0,0,0,dirBuf,mitsDrv,csv0,alv0
	dw	xlate,0,0,0,dirBuf,mitsDrv,csv1,alv1
	dw	xlate,0,0,0,dirBuf,mitsDrv,csv2,alv2
	dw	xlate,0,0,0,dirBuf,mitsDrv,csv3,alv3

;-----------------------------------------------------------------------------
; mitsdrv - disk parameter block. This table gives a block size of 2048 bytes.
;   Per CPM docs, EXM should be 1 and AL0,AL1 should be 080h, 000h.
;   This would give two logical extents per physical extent. (32K per
;   physical extent). The settings here give one logical extent per
;   physical extent (16K per physical extent). This was probably done
;   to maintain compatibility with CPM 1.4 disks.
;-----------------------------------------------------------------------------
mitsDrv	dw	numSect		;sectors per track
	db	4		;allocation block shift factor (BSH)
	db	00fh		;data location block mask (BLM)
	db	0		;extent mask (EXM), see note above
	dw	dsm0		;maximum block number (DSM)	
	dw	drm0		;maximum directory entry number (DRM)
	db	0c0h, 0		;AL0, AL1, see note above
	dw	cks		;CKS (DRM+1)/4
	dw	2		;reserved tracks for CPM and bootloader

;---------------------------------------------------------------------------
; xlate - sector translation table for skew (not a consistent skew step)
;---------------------------------------------------------------------------
xlate	db	01,09,17,25,03,11,19,27,05,13,21,29,07,15,23,31
	db	02,10,18,26,04,12,20,28,06,14,22,30,08,16,24,32

failCnt	db	0		;total disk I/O fail counter

;---------------------------------------------------------------------------
;  flags - bios behavior flags. Some are used in normal operation.
;     Others can be set/cleared for debug.
;---------------------------------------------------------------------------
flags	db	010h		;enable interrupts after disk i/o is done

fCldCmd	equ	001h		;true = CCP process cmd on cold start
fWrmCmd	equ	002h		;true = CCP process cmd on warm start
fRawIO	equ	008h		;r/w directly from altBuf
fEnaInt	equ	010h		;enable interrupts after disk I/O
fWrtVfy	equ	040h		;write verify flag (true = verify)
fTrkVfy	equ	080h		;force track number verification 


;---------------------------------------------------------------------------
; punch - Punch output character BIOS entry point. Sends the character
;    passed in C out the same port as the console.
;---------------------------------------------------------------------------
punch:	jmp	conout


;---------------------------------------------------------------------------
; reader - Reader input character BIOS entry point. Reads a character
;    into A from the same port as the console.
;---------------------------------------------------------------------------
reader:	jmp	sio1In


;---------------------------------------------------------------------------
; const - Console status BIOS entry point. Return 0ff if character ready,
;    return zero otherwise. Coded for the 1st port on a 2SIO board.
;---------------------------------------------------------------------------
const:	in	sio1Ctl		;read 2SIO #1 status/control register
	ani	sioRdrf		;receive data present?
	mvi	a,0
	rz			;no, return zero
	cma			;else return 0ffh
	ret

;---------------------------------------------------------------------------
; sio1In - input character from 1st port on a 2SIO board and return in A
;---------------------------------------------------------------------------
sio1In	in	sio1Ctl		;read 2SIO #1 status/control register
	ani	sioRdrf		;receive data present?
	jz	sio1In		;no, loop until data available
	in	sio1Dat		;read the character
	ani	07fh		;clear msb
	ret

;---------------------------------------------------------------------------
; conout - Console output BIOS entry point. Output the character in C
;    through 2SIO port 1. Blocks more than one C/R in a row.
;---------------------------------------------------------------------------
conout:	in	sio1Ctl		;read 2SIO #1 status/control register
	ani	sioTdre		;transmit data register empty?
	jz	conout		;not yet, keep waiting
	mov	a,c		;going to compare to last char xmitted
	push	h
	lxi	h,lastChr	
	cmp	m		;compare to last character sent
	mov	m,a		;then store as last character sent
	pop	h
	jnz	xmtChar		;if not the same as last, send it
	cpi	cr		;if the same, see if it's another C/R
	rz			;it is, don't sent it 

xmtChar	out	sio1Dat		;send the character
	ret

; lastChr - last character transmitted. Used to prevent multiple CRs in a row

lastChr	db	0		;last character transmitted

;----------------------------------------------------------------------
; listst - List output test BIOS entry point. Return 0ffh if list device
;    is ready, 0 if not ready
;----------------------------------------------------------------------
listst:	in	lpcCtl		;88-LPC board at port 2,3
	ani	lpcBusy		;print busy flag (0=busy)
	mvi	a,0
	rz		
	cma
	ret

;-----------------------------------------------------------------------------
; serinit - init console and punch ports (both ports on an 88-2SIO board)
;-----------------------------------------------------------------------------
serinit: mvi	a,003h		;reset the 6850 ACIA
	out	sio1Ctl
	out	sio2Ctl
	mvi	a,011h		;RTS asserted, no interrupts, 8N2, /16
	out	sio1Ctl
	out	sio2Ctl
	ret

;----------------------------------------------------------------------
; list - List output character BIOS entry point. Send the character
;    passed in C out the list port
;----------------------------------------------------------------------
list:	in	lpcCtl		;88-LPC board at port 2,3
	ani	lpcBusy		;wait for printer (0=busy)		
	jz	list
	mov	a,c
	ani	07fh		;force bit 7 to zero
	out	lpcData		;output the character
	ret

;-----------------------------------------------------------------------------
;  Disk scratchpad areas defined in the DPH table
;-----------------------------------------------------------------------------
dirBuf	ds	128		;bdos directory scratchpad
alv0	ds	20		;((dsm0+1)/8+1)
csv0	ds	cks		;change disk scratchpad
alv1	ds	20		;bdos storage allocation scratchpad
csv1	ds	cks
alv2	ds	20
csv2	ds	cks
alv3	ds	20
csv3	ds	cks

;  End of BIOS equate

biosEnd	equ	($ AND 0fc00h)+0400h+relBias	;round msb up to next 1K boundary

	end

