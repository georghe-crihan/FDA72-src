	OPTION	OFFSET:GROUP
	PUBLIC	RUNSPEC
	EXTRN	__PSP:WORD
	EXTRN	_PUTSTR:FAR
	EXTRN	STR_BUF:BYTE
DGROUP	GROUP	ASMEXT_DATA
ASMEXT_TEXT	SEGMENT	PARA	PUBLIC	'CODE'
	ASSUME	CS:ASMEXT_TEXT,DS:DGROUP,ES:NOTHING,SS:DGROUP
RUNSPEC	PROC	FAR
	PUSH	SI
	PUSH	DI
	MOV	ES,[__PSP]
	XOR	DI,DI
	MOV	ES,ES:[DI+2CH]
	XOR	AX,AX
@@:	MOV	CX,0FFFFH
	REPNE	SCASB
	SCASB
	JNZ	@B
	CMP	WORD PTR ES:[DI],1
	JB	NOPATH
	ADD	DI,2
	PUSH	DI
	MOV	CX,0FFFFH
	REPNE	SCASB
	DEC	DI
	STD
	MOV	AX,'\'
	MOV	CX,0FFFFH
	REPNE	SCASB
	CLD
	ADD	DI,2
	POP	SI
	SUB	DI,SI
	MOV	CX,DI
	LEA	DI,[STR_BUF]
	PUSH	DS
	PUSH	ES
	POP	DS
	POP	ES
	PUSH	CX
	REP	MOVSB
	PUSH	DS
	PUSH	ES
	POP	DS
	POP	ES
	CALL	_PUTSTR
	ADD	SP,2
	POP	DI
	POP	SI
	RET	2
NOPATH:	MOV	AX,DS
	MOV	ES,AX
	MOV	AH,19H
	INT	21H
	ADD	AL,41H
	LEA	DI,[STR_BUF]
	STOSB
	MOV	AX,5C3AH
	STOSW
	MOV	SI,DI
	XOR	DL,DL
	MOV	AH,47H
	INT	21H
	XOR	AL,AL
	MOV	CX,41H
	REPNE	SCASB
	DEC	DI
	CMP	DI,SI
	JE	@F
	MOV	AL,5CH
	STOSB
@@:	SUB	DI,SI
	ADD	DI,3
	PUSH	DI
	CALL	_PUTSTR
	ADD	SP,2
	POP	DI
	POP	SI
	RET	2
RUNSPEC	ENDP
ASMEXT_TEXT	ENDS
ASMEXT_DATA	SEGMENT	WORD	PUBLIC	'DATA'
ASMEXT_DATA	ENDS
	END
