; Input SHA256 : F549A05D3B4BA4D52AD4C7720409AA658FA5F47A1823622E56A190710735BF4F
; Input MD5    : 1385D66B0A5BE906E17193D5B5E4D4A3
; Input CRC32  : 21255B82

; Format      : Object Module Format (OMF/Microsoft)
; Module name      : STRMSB70.asm


                .8086
                .model small

; ===========================================================================

; Segment type: Group
DGROUP          group ASMEXT_DATA

; ===========================================================================

; Segment type: Externs
; extn00
                extrn STRINGADDRESS:far ; CODE XREF: _FINSTR+6↓P
                extrn STRINGASSIGN:far  ; CODE XREF: _GETSTR+15↓P _PUTSTR+14↓P
                extrn STRINGLENGTH:far  ; CODE XREF: _GETSTR+1A↓P
                extrn STRINGRELEASE:far ; CODE XREF: _DELSTR+A↓P

; ===========================================================================

; Segment type: Pure code
ASMEXT_TEXT     segment para public 'CODE'
                assume cs:ASMEXT_TEXT
                assume es:nothing, ss:nothing, ds:DGROUP

; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

                public _FINSTR
_FINSTR         proc far

arg_0           = word ptr  6

                push    bp
                mov     bp, sp
                push    [bp+arg_0]
                call    STRINGADDRESS
                pop     bp
                retf
_FINSTR         endp


; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

                public _GETSTR
_GETSTR         proc far

arg_0           = word ptr  6

                push    bp
                mov     bp, sp
                xor     ax, ax
                mov     bx, [bp+arg_0]
                mov     cx, 100h
                mov     dx, offset STR_BUF
                push    bx
                push    ds
                push    bx
                push    ax
                push    ds
                push    dx
                push    cx
                call    STRINGASSIGN
                call    STRINGLENGTH
                pop     bp
                retf
_GETSTR         endp


; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

                public _PUTSTR
_PUTSTR         proc far

arg_0           = word ptr  6

                push    bp
                mov     bp, sp
                mov     ax, [bp+arg_0]
                mov     bx, offset STR_BUF
                xor     cx, cx
                mov     dx, offset OUT_S_D
                push    ds
                push    bx
                push    ax
                push    ds
                push    dx
                push    cx
                call    STRINGASSIGN
                mov     ax, offset OUT_S_D
                xor     dx, dx
                pop     bp
                retf
_PUTSTR         endp


; =============== S U B R O U T I N E =======================================


                public _DELSTR
_DELSTR         proc far
                mov     bx, offset OUT_S_D
                cmp     word ptr [bx], 0
                jz      short locret_10087
                push    bx
                push    bx
                call    STRINGRELEASE
                pop     bx
                mov     word ptr [bx], 0
                mov     word ptr [bx+2], 0

locret_10087:                           ; CODE XREF: _DELSTR+6↑j
                retf
_DELSTR         endp

; ---------------------------------------------------------------------------
                align 10h
ASMEXT_TEXT     ends

; ===========================================================================

; Segment type: Pure data
ASMEXT_DATA     segment word public 'DATA'
                assume cs:ASMEXT_DATA
                public STR_BUF
STR_BUF         db 100h dup(?)          ; DATA XREF: _GETSTR+B↑o _PUTSTR+6↑o
                public OUT_S_D
OUT_S_D         dd 0                    ; DATA XREF: _PUTSTR+B↑t _PUTSTR+19↑t ...
ASMEXT_DATA     ends


                end
