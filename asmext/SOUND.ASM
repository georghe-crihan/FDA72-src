	OPTION	OFFSET:GROUP
	PUBLIC	ILSOUND
DGROUP	GROUP	ASMEXT_DATA
ASMEXT_TEXT	SEGMENT	PARA	PUBLIC	'CODE'
	ASSUME	CS:ASMEXT_TEXT,DS:DGROUP,ES:NOTHING,SS:DGROUP
ILSOUND	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	BX,[BP+8]
	MOV	CX,[BP+6]
	MOV	AL,10110110B
	OUT	43H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	MOV	AL,BL
	OUT	42H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	MOV	AL,BH
	OUT	42H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	IN	AL,61H
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	PUSH	AX
	OR	AL,00000011B
	OUT	61H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
SOUND1:	MOV	AL,11101000B
	OUT	43H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	IN	AL,42H
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	TEST	AL,AL
	JNS	SOUND1
SOUND2:	MOV	AL,11101000B
	OUT	43H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	IN	AL,42H
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	TEST	AL,AL
	JS	SOUND2
	LOOP	SOUND1
	IN	AL,61H
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	POP	AX
	OUT	61H,AL
	JMP	SHORT	$+2
	JMP	SHORT	$+2
	POP	BP
	RET
ILSOUND	ENDP
ASMEXT_TEXT	ENDS
ASMEXT_DATA	SEGMENT	WORD	PUBLIC	'DATA'
ASMEXT_DATA	ENDS
	END
