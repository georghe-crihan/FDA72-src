	OPTION	OFFSET:GROUP
	PUBLIC	DMABCRE,DMABWR,DMACH2,DMACHK,DMABRD,DMABDEL
	EXTRN	_FINSTR:FAR
DGROUP	GROUP	ASMEXT_DATA
ASMEXT_TEXT	SEGMENT	PARA	PUBLIC	'CODE'
	ASSUME	CS:ASMEXT_TEXT,DS:DGROUP,ES:NOTHING,SS:DGROUP
DMABCRE	PROC	FAR
	PUSH	SI
	PUSH	DI
	MOV	BX,1000H
	MOV	AH,48H
	INT	21H
	JNC	@F
	XOR	AX,AX
	XOR	DX,DX
	JMP	SHORT	L1
@@:	MOV     DX,AX
	MOV	[BUFSEG],DX
	MOV	CX,4
	SHL	DX,CL
	XOR	AX,AX
	ADD	DX,8000H
	JNC	@F
	MOV	AX,8000H
@@:	MOV	[BUFOFF],AX
	MOV	DX,[BUFSEG]
L1:	POP	DI
	POP	SI
	RET
DMABCRE	ENDP
DMABWR	PROC	FAR
	PUSH	BP
	PUSH	SI
	PUSH	DI
	PUSH	DS
	MOV	BP,SP
	PUSH	[BP+0EH]
	CALL	_FINSTR
	ADD	SP,2
	MOV	DI,[BUFOFF]
	ADD	DI,[BP+10H]
	MOV	ES,[BUFSEG]
	MOV	SI,AX
	MOV	DS,DX
	MOV	CX,[BP+0CH]
	REP	MOVSB
	POP	DS
	POP	DI
	POP	SI
	POP	BP
	RET	6
DMABWR	ENDP
DMACH2	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	AX,[BP+0AH]
	OUT	0BH,AL
	JMP	SHORT	$+2
	OUT	0CH,AL
	JMP	SHORT	$+2
	MOV	AX,[BUFSEG]
	MOV	CX,4
	ROL	AX,CL
	MOV	DX,AX
	AND	AX,0FFF0H
	AND	DX,0FH
	ADD	AX,[BUFOFF]
	ADC	DX,0
	ADD	AX,[BP+8]
	ADC	DX,0
	MOV	[BUFLOW],AX
	MOV	[BUFPAGE],DX
	OUT	4,AL
	JMP	SHORT	$+2
	MOV	AL,AH
	OUT	4,AL
	JMP	SHORT	$+2
	MOV	AL,DL
	OUT	81H,AL
	JMP	SHORT	$+2
	MOV	AX,[BP+6]
	DEC	AX
	OUT	5,AL
	JMP	SHORT	$+2
	MOV	AL,AH
	OUT	5,AL
	JMP	SHORT	$+2
	MOV	AL,2
	OUT	0AH,AL
	JMP	SHORT	$+2
	POP	BP
	RET	6
DMACH2	ENDP
DMACHK	PROC	FAR
	MOV	AX,[BUFLOW]
	MOV	DX,[BUFPAGE]
	RET
DMACHK	ENDP
DMABRD	PROC	FAR
	PUSH	BP
	PUSH	SI
	PUSH	DI
	PUSH	DS
	MOV	BP,SP
	PUSH	[BP+0EH]
	CALL	_FINSTR
	ADD	SP,2
	MOV	SI,[BUFOFF]
	ADD	SI,[BP+10H]
	MOV	DS,[BUFSEG]
	MOV	DI,AX
	MOV	ES,DX
	MOV	CX,[BP+0CH]
	REP	MOVSB
	POP	DS
	POP	DI
	POP	SI
	POP	BP
	RET	6
DMABRD	ENDP
DMABDEL	PROC	FAR
	MOV	AH,49H
	MOV	ES,[BUFSEG]
	INT	21H
	RET
DMABDEL	ENDP
ASMEXT_TEXT	ENDS
ASMEXT_DATA	SEGMENT	WORD	PUBLIC	'DATA'
BUFOFF	DW	?
BUFSEG	DW	?
BUFLOW	DW	?
BUFPAGE	DW	?
ASMEXT_DATA	ENDS
	END
