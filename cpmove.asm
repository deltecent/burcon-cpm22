	TITLE	'CP/M VERSION 2.2 SYSTEM RELOCATOR - 2/80'
;	CPM RELOCATOR PROGRAM, INCLUDED WITH THE MODULE TO PERFORM
;	THE MOVE FROM 900H TO THE DESTINATION ADDRESS
;
;	COPYRIGHT (C) 1979
;	DIGITAL RESEARCH
;	BOX 579, PACIFIC GROVE CALIFORNIA
;	93950
;
	ORG	100H
	JMP	PASTCOPY
COPY:	DB	'COPYRIGHT (C) DIGITAL RESEARCH, 1979     '
PASTCOPY:
BIOSWK	EQU	01H	;THREE PAGES FOR BIOS WORKSPACE
STACK	EQU	800H
MODSIZ	EQU	801H	;MODULE SIZE IS STORED HERE
VERSION	EQU	22	;CPM VERSION NUMBER
BOOTSIZ	EQU	3*128	;SIZE OF THE COLD START LOADER
;	(MAY HAVE FIRST 80H BYTES = 00H)
BDOSL	EQU	0800H	;RELATIVE LOCATION OF BDOS
BIOS	EQU	1600H	;RELATIVE LOCATION OF BIOS
;
BOOT	EQU	0000H	;REBOOT LOCATION
BDOS	EQU	0005H
PRNT	EQU	9	;PRINT BUFFER FUNCTION
FCB	EQU	5CH	;DEFAULT FCB
MODULE	EQU	900H	;MODULE ADDRESS
;
CR	EQU	0DH
LF	EQU	0AH
	LXI	SP,STACK
;
;	MAY BE MEMORY SIZE SPECIFIED IN COMMAND
	LXI	D,FCB+1
	LDAX	D
	CPI	' '
	JZ	FINDTOP
	CPI	'?'	;WAS * SPECIFIED?
	JZ	FINDTOP
;
;	MUST BE MEMORY SIZE SPECIFICATION
	LXI	H,0
CLOOP:	;CONVERT TO DECIMAL
	LDAX	D
	INX	D
	CPI	' '
	JZ	ECON
	ORA	A
	JZ	ECON
;	MUST BE DECIMAL DIGIT
	SUI	'0'
	CPI	10
	JNC	CERROR
;	DECIMAL DIGIT IS IN A
	DAD	H	;*2
	PUSH	H
	DAD	H	;*4
	DAD	H	;*8
	POP	B	;*2 IN B,C
	DAD	B	;*10 IN H,L
	MOV	C,A
	MVI	B,0
	DAD	B	;*10+X
	JMP	CLOOP
ECON:	;END OF CONVERSION, CHECK FOR PROPER RANGE
	MOV	A,H
	ORA	A
	JNZ	CERROR
	MOV	A,L
	CPI	16
	JC	CERROR
	MVI	L,0
	MOV	H,A
	DAD	H	;SHL 1
	DAD	H	;SHL 2 FOR KILOBYTES
;	H,L HAVE TOP OF MEMORY+1
	JMP	SETASC
;
CERROR:
	LXI	D,CONMSG
	CALL	PRINT
	JMP	BOOT
CONMSG:	DB	CR,LF,'INVALID MEMORY SIZE$'
;
;
;	FIND END OF MEMORY
FINDTOP:
	LXI	H,0
FINDM:	INR	H	;TO NEXT PAGE
	JZ	MSIZED	;CAN OVERFLOW ON 64K SYSTEMS
	MOV	A,M
	CMA
	MOV	M,A
	CMP	M
	CMA
	MOV	M,A	;BITS INVERTED FOR RAM OPERATIONAL TEST
	JZ	FINDM
;	BITS DIDN'T CHANGE, MUST BE END OF MEMORY
;	ALIGN ON EVEN BOUNDARY
MSIZED:	MOV	A,H
	ANI	1111$1100B	;EVEN 1K BOUNDARY
	MOV	H,A
SETASC:	;SET ASCII VALUE OF MEMORY SIZE
	PUSH	H	;SAVE FOR LATER
	MOV	A,H
	RRC
	RRC
	ANI	11$1111B	;FOR 1K COUNTS
	JNZ	NOT64		;MAY BE 64 K MEM SIZE
	MVI	A,64		;SET TO LITERAL IF SO
NOT64:	MOV	B,A		;READY FOR COUNT DOWN
	LXI	H,AMEM
	MVI	A,'0'
	MOV	M,A
	INX	H
	MOV	M,A	;BOTH ARE SET TO ASCII 0
ASC0:	LXI	H,AMEM+1		;ADDRESS OF ASCII EQUIVALENT
	INR	M
	MOV	A,M
	CPI	'9'+1
	JC	ASC1
	MVI	M,'0'
	DCX	H
	INR	M
ASC1:	DCR	B	;COUNT DOWN BY KILOBYTES
	JNZ	ASC0
	LXI	D,MEMSG
	CALL	PRINT	;MEMORY SIZE MESSAGE
;
	LXI	H,MODSIZ
	MOV	C,M
	INX	H
	MOV	B,M	;B,C CONTAINS MODULE SIZE
	PUSH	B	;MODULE SIZE STACKED ON MEM SIZE
;
;	TRY TO FIND THE ASCII STRING 'K CP/M VER X.X' TO SET SIZE
	LXI	H,MODULE
;	B,C CONTAINS MODULE LENGTH
SLOOP:	;SEARCH LOOP
	LXI	D,AMSG
	MOV	A,B
	ORA	C
	JZ	ESEAR	;END OF SEARCH
	DCX	B	;COUNT SEARCH LENGTH DOWN
	PUSH	B
	MVI	C,LAMSG	;LENGTH OF SEARCH MESSAGE
	PUSH	H	;SAVE BASE ADDRESS OF SEARCH
CHLOOP:	;CHARACTER LOOP, MATCH ON CONTENTS OF D,E AND H,L
	LDAX	D
	CMP	M
	JNZ	NOMATCH
	INX	D	;TO NEXT SEARCH CHARACTER
	INX	H	;TO NEXT MATCH CHARACTER
	DCR	C	;COUNT LENGTH DOWN
	JZ	FSEAR	;FOUND SEARCH STRING
	JMP	CHLOOP
;
NOMATCH:
	;NOT FOUND AT THIS ADDRESS, LOOK AT NEXT ADDRESS
	POP	H
	INX	H
	POP	B	;RECALL MODULE LENGTH
	JMP	SLOOP
;
FSEAR:
	;FOUND STRING, SET MEMORY SIZE
	POP	H	;START ADDRESS OF STRING BEING MATCHED
	POP	B	;CLEAR B,C WHICH WAS STACKED
	DCX	H
	LXI	D,AMEM+1
	LDAX	D
	MOV	M,A
	DCX	H
	DCX	D
	LDAX	D
	MOV	M,A
;	END OF FILL
;
ESEAR:	;END OF SEARCH
	POP	B	;RECOVER MODULE LENGTH
	POP	H	;H,L CONTAINS END OF MEMORY
	PUSH	B	;SAVE LENGTH FOR RELOCATION BELOW
	MOV	A,B
	ADI	BIOSWK	;ADD BIOS WORK SPACE TO MODULE LENGTH
	MOV	B,A
	MOV	A,L
	SUB	C	;COMPUTE MEMTOP-MODULE SIZE
	MOV	L,A
	MOV	A,H
	SBB	B
	MOV	H,A
;	H,L CONTAINS THE BASE OF THE RELOCATION AREA
	SHLD	RELBAS	;SAVE THE RELOCATION BASE
	XCHG		;MODULE BASE TO D,E
	LXI	H,MODULE;READY FOR THE MOVE
	POP	B	;RECOVER ACTUAL MODULE LENGTH
	PUSH	B	;SAVE FOR RELOCATION
	DAD	B	;MOVE H,L TO BIT MAP POSITION
	JMP	RELOC
;
MOVE:
	MOV	A,B	;BC=0?
	ORA	C
	JZ	RELOC
	DCX	B	;COUNT MODULE SIZE DOWN TO ZERO
	MOV	A,M	;GET NEXT ABSOLUTE LOCATION
	STAX	D	;PLACE IT INTO THE RELOC AREA
	INX	D
	INX	H
	JMP	MOVE
;
RELOC:	;STORAGE MOVED, READY FOR RELOCATION
;	HL ADDRESSES BEGINNING OF THE BIT MAP FOR RELOCATION
	POP	B	;RECALL MODULE LENGTH
	PUSH	H	;SAVE BIT MAP BASE IN STACK
	LHLD	RELBAS
	XCHG
	LXI	H,BOOTSIZ
	DAD	D	;TO FIND BIAS VALUE
;	REGISTER H CONTAINS BIAS VALUE
;
	LXI	D,MODULE
REL0:	MOV	A,B	;BC=0?
	ORA	C
	JZ	ENDREL
;
;	NOT END OF THE RELOCATION, MAY BE INTO NEXT BYTE OF BIT MAP
	DCX	B	;COUNT LENGTH DOWN
	MOV	A,E
	ANI	111B	;0 CAUSES FETCH OF NEXT BYTE
	JNZ	REL1
;	FETCH BIT MAP FROM STACKED ADDRESS
	XTHL
	MOV	A,M	;NEXT 8 BITS OF MAP
	INX	H
	XTHL		;BASE ADDRESS GOES BACK TO STACK
	MOV	L,A	;L HOLDS THE MAP AS WE PROCESS 8 LOCATIONS
REL1:	MOV	A,L
	RAL		;CY SET TO 1 IF RELOCATION NECESSARY
	MOV	L,A	;BACK TO L FOR NEXT TIME AROUND
	JNC	REL2	;SKIP RELOCATION IF CY=0
;
;	CURRENT ADDRESS REQUIRES RELOCATION
	LDAX	D
	ADD	H	;APPLY BIAS IN H
	STAX	D
	JMP	REL2
;
REL2:	INX	D	;TO NEXT ADDRESS
	JMP	REL0	;FOR ANOTHER BYTE TO RELOCATE
;
ENDREL:	;END OF RELOCATION
	POP	D	;CLEAR STACKED ADDRESS
;	DON'T GO TO THE LOADED PROGRAM, LEAVE IN MEMORY
;	MAY HAVE TO MOVE THE PROGRAM IMAGE DOWN 1/2 PAGE
	MVI	B,128	;CHECK FOR 128 ZEROES
	LXI	H,MODULE
TR0:	MOV	A,M
	ORA	A
	JNZ	TREND
	INX	H
	DCR	B
	JNZ	TR0
;
;	ALL ZERO FIRST 1/2 PAGE, MOVE DOWN 80H BYTES
	XCHG		;NEXT TO GET IN D,E
	LHLD	MODSIZ
	LXI	B,-128
	DAD	B	;NUMBER OF BYTES TO MOVE IN H,L
	MOV	B,H
	MOV	C,L	;TRANSFERRED TO B,C
	LXI	H,MODULE;DESTINATION IN H,L
TRMOV:	MOV	A,B
	ORA	C	;ALL MOVED?
	JZ	TREND
	DCX	B
	LDAX	D
	MOV	M,A	;ONE BYTE TRANSFERRED
	INX	D
	INX	H
	JMP	TRMOV
;
TREND:	;SET ASCII MEMORY IMAGE SIZE
	LXI	H,MODSIZ
	MOV	C,M
	INX	H
	MOV	B,M
	LXI	H,MODULE;B,C MODULE SIZE, H,L BASE
	DAD	B
	MOV	A,H
	ADI	1
	MOV	B,A	;B CONTAINS NUMBER OF PAGES TO SAVE+1
	LXI	H,SAVMEM;ASCII MEMORY SIZE
	MVI	A,'0'
	MOV	M,A
	INX	H
	MOV	M,A
;	'00' STORED INTO MESSAGE
TRCOMP:
	DCR	B
	JZ	TRC1
	LXI	H,SAVMEM+1	;ADDRESSING LEAST DIGIT
	INR	M
	MOV	A,M
	CPI	'9'+1
	JC	TRCOMP
	MVI	M,'0'
	DCX	H
	INR	M
	JMP	TRCOMP
;	FILL CPMXX.COM FROM SAVMEM
TRC1:	LHLD	AMEM
	SHLD	SAVM0
;	MESSAGE SET, PRINT IT AND REBOOT
	LXI	D,RELOK
	CALL	PRINT
	JMP	BOOT
RELOK:	DB	CR,LF,'READY FOR "SYSGEN" OR'
	DB	CR,LF,'"SAVE '
SAVMEM:	DB	'00 CPM'
SAVM0:	DB	'00.COM"$'
;
TRANSFER:
;	GO TO THE RELOCATED MEMORY IMAGE
	LXI	D,BOOTSIZ+BIOS	;MODULE 
	LHLD	RELBAS	;RECALL BASE OF RELOC AREA
	DAD	D	;INDEX TO 'BOOT' ENTRY POINT
	PCHL		;GO TO RELOCATED PROGRAM
;
PRINT:
	MVI	C,PRNT
PRJMP:	JMP	BDOS
;
;	DATA	AREAS
RELBAS:	DS	2	;RELOCATION BASE
MEMSG:	DB	CR,LF,'CONSTRUCTING '
AMEM:	DB	'00'
AMSG:	DB	'k CP/M vers '
	DB	VERSION/10+'0','.',VERSION MOD 10 +'0'
LAMSG	EQU	$-AMSG	;LENGTH OF MESSAGE
	DB	'$'		;TERMINATOR FOR MESSAGE
	END
