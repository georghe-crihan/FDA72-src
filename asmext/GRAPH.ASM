	OPTION	OFFSET:GROUP,EXPR32
	PUBLIC	GRAPHINIT,GRAPHCLEAN
	PUBLIC	GRAPHCURS,GRAPHCURR
	PUBLIC	GRAPHLINE,GRAPHLOCATE,GRAPHPRINT,GRAPHCLS
	PUBLIC	GRAPHSCRPR,GRAPHSCRBMP,GRAPHRDBMP
	EXTRN	_GETSTR:FAR,STR_BUF:BYTE
DGROUP	GROUP	ASMEXT_DATA
ASMEXT_TEXT	SEGMENT	PARA	PUBLIC	'CODE'
	ASSUME	CS:ASMEXT_TEXT,DS:DGROUP,ES:NOTHING,SS:DGROUP
GRAPHINIT	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	AH,0FH
	PUSH	BP
	INT	10H
	POP	BP
	MOV	[DOSVMOD],AL
	MOV	AX,[BP+8]
	TEST	AX,AX
	JNZ	@F
	PUSH	AX
	MOV	AX,[BP+6]
	XOR	AH,AH
	PUSH	BP
	INT	10H
	POP	BP
	POP	AX
@@:	TEST	AX,AX
	JZ	@F
	PUSH	AX
	MOV	AX,4F02H
	MOV	BX,[BP+6]
	PUSH	BP
	INT	10H
	POP	BP
	POP	AX
@@:	POP	BP
	RET	4
GRAPHINIT	ENDP
GRAPHCLEAN	PROC	FAR
	MOV	AL,[DOSVMOD]
	XOR	AH,AH
	PUSH	BP
	INT	10H
	POP	BP
	RET
GRAPHCLEAN	ENDP
GRAPHCURS	PROC	FAR
	PUSH	BP
	PUSH	SI
	PUSH	DI
	MOV	BP,SP
	MOV	DX,[BP+0EH]
	LEA	SI,[CURMASK]
	LEA	DI,[SAVEBUF]
	MOV	AX,DS
	MOV	ES,AX
	MOV	CX,14H
GCSRSL:	LODSW
	PUSH	CX
	MOV	BX,10H
	MOV	CX,[BP+0CH]
GCSCSL:	PUSH	BX
	PUSH	AX
	MOV	AH,0DH
	XOR	BH,BH
	PUSH	BP
	INT	10H
	POP	BP
	STOSB
	POP	AX
	SHL	AX,1
	JNC	@F
	PUSH	AX
	MOV	AL,[BP+0AH]
	MOV	AH,0CH
	XOR	BH,BH
	PUSH	BP
	INT	10H
	POP	BP
	POP	AX
@@:	POP	BX
	INC	CX
	DEC	BX
	JNZ	GCSCSL
	POP	CX
	INC	DX
	LOOP	GCSCSL
	POP	DI
	POP	SI
	POP	BP
	RET	6
GRAPHCURS	ENDP
GRAPHCURR	PROC	FAR
	PUSH	BP
	PUSH	SI
	MOV	BP,SP
	MOV	DX,[BP+0AH]
	LEA	SI,[SAVEBUF]
	MOV	CX,14H
GCRRSL:	LODSW
	PUSH	CX
	MOV	BX,10H
	MOV	CX,[BP+8]
GCRCSL:	PUSH	BX
	LODSB
	MOV	AH,0CH
	XOR	BH,BH
	PUSH	BP
	INT	10H
	POP	BP
	POP	BX
	INC	CX
	DEC	BX
	JNZ	GCRCSL
	POP	CX
	INC	DX
	LOOP	GCRCSL
	POP	SI
	POP	BP
	RET	4
GRAPHCURR	ENDP
GRAPHLINE	PROC	FAR
	PUSH	BP
	PUSH	SI
	PUSH	DI
	MOV	BP,SP
	MOV	AX,[BP+0EH]
	OR	AX,[BP+0CH]
	TEST	AX,AX
	JNZ	@F
	MOV	AL,[BP+0AH]
	MOV	AH,0CH
	XOR	BH,BH
	MOV	CX,[BP+10H]
	MOV	DX,[BP+12H]
	INT	10H
	JMP	GLNDON
@@:	MOV	AX,[BP+0CH]
	XOR	DI,DI
	TEST	AX,AX
	JNS	@F
	NEG	AX
	MOV	DI,1
@@:	MOV	[XLEN],AX
	MOV	AX,[BP+0EH]
	XOR	DX,DX
	TEST	AX,AX
	JNS	@F
	NEG	AX
	MOV	DX,1
@@:	MOV	[YLEN],AX
	MOV	[YFLAG],DX
	MOV	[SWAPFLG],0
	CMP	AX,[XLEN]
	JBE	@F
	XCHG	AX,[XLEN]
	MOV	[YLEN],AX
	XCHG	DI,[YFLAG]
	MOV	[SWAPFLG],1
@@:	XOR	SI,SI
GLNLDL:	MOV	CX,SI
	TEST	DI,DI
	JZ	@F
	NEG	CX
@@:	MOV	AX,SI
	MUL	[YLEN]
	MOV	BX,[XLEN]
	DIV	BX
	SHR	BX,1
	CMP	DX,BX
	JB	@F
	INC	AX
@@:	CMP	[YFLAG],0
	JZ	@F
	NEG	AX
@@:	MOV	DX,AX
	CMP	[SWAPFLG],0
	JZ	@F
	XCHG	CX,DX
@@:	ADD	CX,[BP+10H]
	ADD	DX,[BP+12H]
	MOV	AL,[BP+0AH]
	MOV	AH,0CH
	XOR	BH,BH
	PUSH	BP
	INT	10H
	POP	BP
	INC	SI
	CMP	SI,[XLEN]
	JBE	GLNLDL
GLNDON:	POP	DI
	POP	SI
	POP	BP
	RET	0AH
GRAPHLINE	ENDP
GRAPHLOCATE	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	AH,2
	XOR	BH,BH
	MOV	DL,[BP+6]
	MOV	DH,[BP+8]
	INT	10H
	POP	BP
	RET	4
GRAPHLOCATE	ENDP
GRAPHPRINT	PROC	FAR
	PUSH	BP
	PUSH	SI
	MOV	BP,SP
	PUSH	[BP+0AH]
	CALL	_GETSTR
	ADD	SP,2
	MOV	CX,AX
	LEA	SI,[STR_BUF]
@@:	LODSB
	MOV	AH,0EH
	MOV	BL,[BP+8]
	XOR	BH,BH
	PUSH	BP
	INT	10H
	POP	BP
	LOOP	@B
	POP	SI
	POP	BP
	RET	4
GRAPHPRINT	ENDP
GRAPHCLS	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	AX,600H
	XOR	BH,BH
	MOV	CH,[BP+0CH]
	MOV	CL,[BP+0AH]
	MOV	DH,[BP+8]
	MOV	DL,[BP+6]
	INT	10H
	MOV	AH,2
	MOV	DX,CX
	INT	10H
	POP	BP
	RET	8
GRAPHCLS	ENDP
GRAPHSCRPR	PROC	FAR
	PUSH	BP
	PUSH	SI
	PUSH	DI
	MOV	[ERRFLAG],0
	MOV	AX,3524H
	INT	21H
	PUSH	BX
	PUSH	ES
	MOV	AX,2524H
	PUSH	DS
	MOV	DX,CS
	MOV	DS,DX
	LEA	DX,[HOOK24]
	INT	21H
	POP	DS
	MOV	AH,0FH
	INT	10H
	CMP	AL,12H
	JE	@F
	MOV	[ERRFLAG],1
	JMP	DONE
@@:	MOV	AH,40H
	MOV	BX,4
	MOV	CX,0CH
	LEA	DX,[ONCETBL]
	INT	21H
	JC	DONE
	MOV	[PROWCNT],0
ROWCYC:	MOV	AH,40H
	MOV	BX,4
	MOV	CX,185
	LEA	DX,[INITTBL]
	INT	21H
	JC	DONE
	MOV	[PCOLCNT],0
COLCYC:	MOV	[PBYTCNT],0
	MOV	AX,[PROWCNT]
	MOV	[PBYTPTR],AX
BYTCYC:	MOV	[PBITCNT],0
	MOV	AX,[PBYTPTR]
	MOV	[PBITPTR],AX
	MOV	[OURBYTE],0
	MOV	[OURMASK],0C0H
BITCYC:	MOV	AH,0DH
	XOR	BH,BH
	MOV	CX,[PCOLCNT]
	MOV	DX,[PBITPTR]
	INT	10H
	TEST	AL,AL
	JZ	@F
	MOV	AL,[OURMASK]
	OR	[OURBYTE],AL
@@:	SHR	[OURMASK],1
	SHR	[OURMASK],1
	INC	[PBITPTR]
	INC	[PBITCNT]
	CMP	[PBITCNT],4
	JB	BITCYC
	MOV	AH,40H
	MOV	BX,4
	MOV	CX,1
	LEA	DX,[OURBYTE]
	INT	21H
	JC	DONE
	ADD	[PBYTPTR],4
	INC	[PBYTCNT]
	CMP	[PBYTCNT],3
	JB	BYTCYC
	INC	[PCOLCNT]
	CMP	[PCOLCNT],640
	JB	COLCYC
	MOV	AH,40H
	MOV	BX,4
	MOV	CX,2
	LEA	DX,[CRLFTBL]
	INT	21H
	JC	DONE
	ADD	[PROWCNT],12
	CMP	[PROWCNT],480
	JB	ROWCYC
	MOV	AH,40H
	MOV	BX,4
	MOV	CX,2
	LEA	DX,[EXITTBL]
	INT	21H
DONE:	MOV	CX,DS
	POP	DS
	POP	DX
	MOV	AX,2524H
	INT	21H
	MOV	DS,CX
	MOV	AL,[ERRFLAG]
	XOR	AH,AH
	POP	DI
	POP	SI
	POP	BP
	RET
GRAPHSCRPR	ENDP
GRAPHSCRBMP	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	PUSH	SI
	PUSH	DI
	MOV	AH,0FH
	INT	10H
	CMP	AL,12H
	JNE	DONEER
	PUSH	[BP+6]
	CALL	_GETSTR
	ADD	SP,2
	LEA	BX,[STR_BUF]
	ADD	BX,AX
	MOV	BYTE PTR DS:[BX],0
	MOV	AH,3CH
	XOR	CX,CX
	LEA	DX,[STR_BUF]
	INT	21H
	JC	DONEER
	MOV	[BMFHAND],AX
	MOV	AH,40H
	MOV	BX,[BMFHAND]
	MOV	CX,BMHDRLN
	LEA	DX,[BMAPHDR]
	INT	21H
	JNC	@F
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	JMP	DONEER
@@:	MOV	AX,DS
	MOV	ES,AX
	MOV	[PROWCNT],480
ROWSLP:	DEC	[PROWCNT]
	LEA	DI,[BMPLINE]
	MOV	[PCOLCNT],0
	MOV	[PBITPTR],0
COLSLP:	MOV	[PBITCNT],0
	MOV	[OURBYTE],0
NIBLLP:	MOV	AH,0DH
	XOR	BH,BH
	MOV	CX,[PBITPTR]
	MOV	DX,[PROWCNT]
	INT	10H
	CMP	[PBITCNT],0
	JNZ	@F
	MOV	CL,4
	SHL	AL,CL
@@:	OR	[OURBYTE],AL
	INC	[PBITPTR]
	INC	[PBITCNT]
	CMP	[PBITCNT],2
	JB	NIBLLP
	MOV	AL,[OURBYTE]
	STOSB
	INC	[PCOLCNT]
	CMP	[PCOLCNT],320
	JB	COLSLP
	MOV	AH,40H
	MOV	BX,[BMFHAND]
	MOV	CX,320
	LEA	DX,[BMPLINE]
	INT	21H
	JNC	@F
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	JMP	DONEER
@@:	CMP	[PROWCNT],0
	JNZ	ROWSLP
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	XOR	AX,AX
	JMP	DONEOK
DONEER:	MOV	AX,1
DONEOK:	POP	DI
	POP	SI
	POP	BP
	RET	2
GRAPHSCRBMP	ENDP
GRAPHRDBMP	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	PUSH	SI
	PUSH	DI
	MOV	AH,0FH
	INT	10H
	CMP	AL,12H
	JNE	DONEER1
	PUSH	[BP+6]
	CALL	_GETSTR
	ADD	SP,2
	LEA	BX,[STR_BUF]
	ADD	BX,AX
	MOV	BYTE PTR DS:[BX],0
	MOV	AX,3D20H
	LEA	DX,[STR_BUF]
	INT	21H
	JC	DONEER1
	MOV	[BMFHAND],AX
	MOV	AX,4200H
	MOV	BX,[BMFHAND]
	XOR	CX,CX
	MOV	DX,BMHDRLN
	INT	21H
	JNC	@F
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	JMP	DONEER1
@@:	MOV	[PROWCNT],480
ROWSLP1:
	DEC	[PROWCNT]
	MOV	AH,3FH
	MOV	BX,[BMFHAND]
	MOV	CX,320
	LEA	DX,[BMPLINE]
	INT	21H
	JNC	@F
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	JMP	DONEER1
@@:	LEA	SI,[BMPLINE]
	MOV	[PCOLCNT],0
	MOV	[PBITPTR],0
COLSLP1:
	MOV	[PBITCNT],0
NIBLLP1:
	MOV	AL,BYTE PTR DS:[SI]
	CMP	[PBITCNT],0
	JNZ	@F
	MOV	CL,4
	SHR	AL,CL
@@:	AND	AL,0FH
	MOV	AH,0CH
	XOR	BH,BH
	MOV	CX,[PBITPTR]
	MOV	DX,[PROWCNT]
	INT	10H
	INC	[PBITPTR]
	INC	[PBITCNT]
	CMP	[PBITCNT],2
	JB	NIBLLP1
	INC	SI
	INC	[PCOLCNT]
	CMP	[PCOLCNT],320
	JB	COLSLP1
	CMP	[PROWCNT],0
	JNZ	ROWSLP1
	MOV	AH,3EH
	LEA	BX,[BMFHAND]
	INT	21H
	XOR	AX,AX
	JMP	DONEOK1
DONEER1:
	MOV	AX,1
DONEOK1:
	POP	DI
	POP	SI
	POP	BP
	RET	2
GRAPHRDBMP	ENDP
HOOK24:	STI
	PUSH	AX
	PUSH	DS
	MOV	AX,DGROUP
	MOV	DS,AX
	MOV	[ERRFLAG],1
	POP	DS
	POP	AX
	MOV	AL,03H
	IRET
ASMEXT_TEXT	ENDS
ASMEXT_DATA	SEGMENT	WORD	PUBLIC	'DATA'
DOSVMOD	DB	?
CURMASK	DB	000H,000H
	DB	040H,000H
	DB	060H,000H
	DB	070H,000H
	DB	078H,000H
	DB	07CH,000H
	DB	07EH,000H
	DB	07FH,000H
	DB	07FH,080H
	DB	07FH,0C0H
	DB	07EH,000H
	DB	076H,000H
	DB	066H,000H
	DB	043H,000H
	DB	003H,000H
	DB	001H,080H
	DB	001H,080H
	DB	000H,0C0H
	DB	000H,0C0H
	DB	000H,000H
SAVEBUF	DB	320 DUP(?)
XLEN	DW	?
YLEN	DW	?
YFLAG	DW	?
SWAPFLG	DB	?
PROWCNT	DW	?
PCOLCNT	DW	?
PBYTCNT	DW	?
PBITCNT	DW	?
PBYTPTR	DW	?
PBITPTR	DW	?
OURBYTE	DB	?
OURMASK	DB	?
ERRFLAG	DB	?
ONCETBL	DB	1BH,40H,1BH,6CH,0,1BH,33H,24,4 DUP(0AH)
INITTBL	DB	1BH,'*',38,0BCH,2,180 DUP(0)
CRLFTBL	DB	0DH,0AH
EXITTBL	DB	1BH,40H
BMAPHDR	DB	'BM'				; DIB file signature
	DD	BMHDRLN+320*480			; DIB file size in bytes
	DW	0,0				; Marked as reserved in the SDK
	DD	BMHDRLN				; Offset of actual data from
						;   the beginning of the file
BMINFHD	DD	BMINFHL				; Size of BITMAPINFOHEADER
	DD	640,480				; Width and height in pixels
	DW	1,4				; planes and bits per pixel
	DD	0				; BI_RGB (no compression)
	DD	0				; Don't have to specify image
						;   size here
	DD	0,0				; Don't specify pels per meter
	DD	0,0				; Don't specify colors used and
						;   colors important
BMINFHL	EQU	$-BMINFHD
	DB	0FFH,0FFH,0FFH,0		; And now the color table.
	DB	0FFH,0FFH,0FFH,0		;   Since the only video mode
	DB	0FFH,0FFH,0FFH,0		;   we support is VGA-specific,
	DB	0FFH,0FFH,0FFH,0		;   we don't have to worry
	DB	0FFH,0FFH,0FFH,0		;   about different adapters
	DB	0FFH,0FFH,0FFH,0		;   monitors, and we can define
	DB	0FFH,0FFH,0FFH,0		;   the colors the way VGA
	DB	0AAH,0AAH,0AAH,0		;   does.
	DB	0FFH,0FFH,0FFH,0
	DB	0FFH,0FFH,0FFH,0
	DB	0FFH,0FFH,0FFH,0
	DB	0FFH,000H,000H,0
	DB	000H,000H,0FFH,0
	DB	0FFH,000H,0FFH,0
	DB	000H,0C0H,0C0H,0
	DB	000H,000H,000H,0
BMHDRLN	EQU	$-BMAPHDR
BMFHAND	DW	?
BMPLINE	DB	320 DUP(?)
ASMEXT_DATA	ENDS
	END
