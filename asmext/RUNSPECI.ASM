	OPTION	OFFSET:GROUP
	PUBLIC	RUNSPEC,OWUPDATE
	EXTRN	_GETSTR:FAR,_PUTSTR:FAR
	EXTRN	STR_BUF:BYTE
DGROUP	GROUP	ASMEXT_DATA
ASMEXT_TEXT	SEGMENT	PARA	PUBLIC	'CODE'
	ASSUME	CS:ASMEXT_TEXT,DS:DGROUP,ES:NOTHING,SS:DGROUP
RUNSPEC	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	PUSH	[BP+6]
	CALL	_GETSTR
	ADD	SP,2
	PUSH	AX
	CALL	_PUTSTR
	ADD	SP,2
	POP	BP
	RET	2
RUNSPEC	ENDP
OWUPDATE	PROC	FAR
	XOR	AX,AX
	RET	8
OWUPDATE	ENDP
ASMEXT_TEXT	ENDS
ASMEXT_DATA	SEGMENT	WORD	PUBLIC	'DATA'
ASMEXT_DATA	ENDS
	END
