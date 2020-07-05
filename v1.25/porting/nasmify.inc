; nasm macros to handle the odd assembly language syntax used in MS-DOS
; v1.25's ASM.ASM, HEX2BIN.ASM, and TRANS.ASM.
;
;	Copyright (c) 2020 TK Chia.
;
;	Permission is hereby granted, free of charge, to any person
;	obtaining a copy of this software and associated documentation files
;	(the "Software"), to deal in the Software without restriction,
;	including without limitation the rights to use, copy, modify, merge,
;	publish, distribute, sublicense, and/or sell copies of the Software,
;	and to permit persons to whom the Software is furnished to do so,
;	subject to the following conditions:
;
;	The above copyright notice and this permission notice shall be
;	included in all copies or substantial portions of the Software.
;
;	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;	NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;	BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;	ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;	CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;	SOFTWARE.

%define	CMPB	cmpsb
%define	CMPW	cmpsw
%define	LODB	lodsb
%define	LODW	lodsw
%define	MOVB	movsb
%define	MOVW	movsw
%define	SCAB	scasb
%define	SCAW	scasw
%define	STOB	stosb
%define	STOW	stosw

%define	ALIGN	$ALIGN
%define	DEFAULT	$DEFAULT
%define	ORG	$ORG
%define	RCL	$RCL
%define	RCR	$RCR
%define	RAX	$RAX
%define	RBX	$RBX
%define	RCX	$RCX
%define	RDX	$RDX
%define	RSI	$RSI
%define	RDI	$RDI

%macro	$ALIGN	0
	alignb	2
%endmacro

%macro	AND	3
	and	byte %2,%3
%endmacro

%macro	CMP	2
%ifidn	{''''},{%2}
	cmp	%1,"'"
%eliftoken %1
	cmp	%1,%2
%else
	cmp	word %1,%2
%endif
%endmacro

; E.g.
;	CMP	B,[SYM],")"
%macro	CMP	3
	cmp	byte %2,%3
%endmacro

%macro	DEC	2
	dec	byte %2
%endmacro

; DM is like DB, except it sets the most significant bit of the last output
; byte.  E.g.
;	DM	"ret"
; is akin to
;	DB	"re",80h|"t"
; The high bit is used to mark the end of an ASCII string.
%macro	DM	1-*
%rep	%0-1
	db	%1
%rotate	1
%endrep
%ifnum	%1
	db	80h|(%1)
%else
%strlen	%%len	%1
%assign	%%i	1
%rep	%%len-1
%substr	%%ch	%1 %%i
	db	%%ch
%assign	%%i	%%i+1
%endrep
%substr	%%ch	%1 %%i
	db	80h|(%%ch)
%endif
%endmacro

%macro	DIV	2
	div	%2
%endmacro

%macro	DS	1
	resb	%1
%endmacro

%macro	INC	1
%iftoken %1
	inc	%1
%else
	inc	WORD %1
%endif
%endmacro

%macro	INC	2
	inc	byte %2
%endmacro

%macro	JMP	1
%iftoken %1
	jmp	near %1
%elifidn {[BX]},{%1}
	jmp	word %1
%else
	$JMP%1
%endif
%endmacro

%macro	JMPS	1
	jmp	short %1
%endmacro

%macro	JP	1
	jmp	short %1
%endmacro

;	JC	RET
; means to output a conditional jump to a nearby (?) RET instruction.
%macro	?j.cc	2
%ifnidn	{RET},{%1}
	%2	%1
%elifndef ?nxtret
	%2	short ?ret0
%else
%assign	?curret	?nxtret-1
%if	$+2-?ret%[?curret]<=127
	%2	short ?ret%[?curret]
%else
	%2	short ?ret%[?nxtret]
%endif
%endif
%endmacro

%macro	JC	1
	?j.cc	%1,jc
%endmacro

%macro	JNC	1
	?j.cc	%1,jnc
%endmacro

%macro	JNZ	1
	?j.cc	%1,jnz
%endmacro

%macro	JZ	1
	?j.cc	%1,jz
%endmacro

%macro	MOV	2
%iftoken %1
	mov	%1,%2
%elifnum %2
	mov	word %1,%2
%elifidn {AL},{%2}
	mov	%1,%2
%elifidn {AH},{%2}
	mov	%1,%2
%elifidn {BL},{%2}
	mov	%1,%2
%elifidn {BH},{%2}
	mov	%1,%2
%elifidn {CL},{%2}
	mov	%1,%2
%elifidn {CH},{%2}
	mov	%1,%2
%elifidn {DL},{%2}
	mov	%1,%2
%elifidn {DH},{%2}
	mov	%1,%2
%elifid	%2
	mov	word %1,%2
%else
	mov	%1,%2
%endif
%endmacro

%macro	MOV	3
	mov	byte %2,%3
%endmacro

%macro	MUL	2
	mul	%2
%endmacro

%macro	OR	3
	or	byte %2,%3
%endmacro

%macro	$ORG	1
%ifidn {:},{%1}
	$ORG:
%elifndef ?org
%assign	?org	%1
	org	%1
%else
	absolute %1
%endif
%endmacro

%macro	PUT	1
%ifidn	{:},{%1}
	$PUT:
%endif
%endmacro

%macro	$RCL	1
%iftoken %1
	rcl	%1,1
%else
; Handle this:
; RCL:	DM	"RCL"
	$RCL%1
%endif
%endmacro

%macro	$RCR	1
%iftoken %1
	rcr	%1,1
%else
	$RCR%1
%endif
%endmacro

%macro	ROL	1
%iftoken %1
	rol	%1,1
%else
	$ROL%1
%endif
%endmacro

%macro	RET	0
%ifndef	?nxtret
%assign	?nxtret	0
%endif
?ret%[?nxtret]:
	ret
%assign	?nxtret	?nxtret+1
%endmacro

%macro	SEG	1
	%1
%endmacro

%macro	SHL	1
	shl	%1,1
%endmacro

%macro	SHR	1
%iftoken %1
	shr	%1,1
%else
	$SHR%1
%endif
%endmacro

; Hack for the last line of HEX2BIN.ASM.
%macro	STARTSEG 1
$STARTSEG EQU	(START-$$+?org+15)/16
%endmacro

%macro	TEST	2
%iftoken %1
	test	%1,%2
%else
	test	word %1,%2
%endif
%endmacro

%macro	TEST	3
	test	byte %2,%3
%endmacro

%macro	UP	0
	cld
%endmacro
