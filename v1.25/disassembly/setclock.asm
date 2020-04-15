; MS-DOS v1.25 SETCLOCK.COM
; SHA-256: 2ad14b49a9206e47a6942c9e2bc738c00d804d34cb8d693312228cf49820e508
; Binary is part of MS-DOS v1.25 code as released by MS under MIT License
; Use the Netwide Assembler (nasm) to reproduce the original program

; This is apparently a Terminate-and-Stay-Resident (TSR) program which reads
; and sets a particular type of hardware real time clock.

	bits	16

	org	0x0100

LF		equ	10
CR		equ	13

AST_BASE_PORT	equ	0x2c0
exit		equ	$-0x0100
zero_flags	equ	$-2

start:
	jmp	main

; Original int 0x21 vector.
org_int_0x21:
	dd	0

; New int 0x21 handling routine.
new_int_0x21:
	cmp	ah,0x2b			; If ah is neither 0x2b (set system
	jae	.may_set_date_time	; date) nor 0x2d (set system time),
.ignore:				; just hand over to the original
	jmp	far [cs:org_int_0x21]	; int 0x21 routine
.may_set_date_time:
	jz	.set_date_time
	cmp	ah,0x2d
	jnz	.ignore
.set_date_time:
	push	ax
	push	word [cs:zero_flags]	; First, call the old int 0x21 ISR
	call	far [cs:org_int_0x21]	; to set BIOS's & DOS's idea of the
					; system date or time
	cmp	al,0xff			; If the request failed, just return
	jnz	.dos_ok			; immediately
	add	sp,byte 2
	iret
.dos_ok:				; The request worked --- also set
	pop	ax			; the real time clock's date or time
	push	bx
	push	cx
	push	dx
	push	bp
	push	si
    dw 0xf18b ; mov si,cx		; si <- year number (if date) or
					;	<hour, minute> (if time)
	mov	bp,AST_BASE_PORT	; Set bp to the base port address
					; for the AST clock
	push	dx			; Save dx: this is either <month,
					; day> or <second, 100ths of second>
	lea	dx,[bp+1]
	mov	cl,1			; cx <- 0 if setting date,
					;	non-zero if setting time
	cmp	ah,0x2d
	jz	.set_ast_time
	lea	dx,[bp+0xa]		; ah = 0x2b case (set date):
	xchg	ax,si
	sub	ax,1980			; Port 0x2ca <- year number - 1980
	out	dx,al
	pop	ax			; ..
	push	ax			;  : al <- month
    dw 0xc48a ; mov al,ah		; .:
	dec	dx			; Port 0x2c9 <- month (binary)
	out	dx,al
	lea	dx,[bp+6]
    dw 0xc92b ; sub cx,cx		; cx <- 0, i.e. set date
.set_ast_time:
	pop	ax			; ..
	push	ax			;  : al <- day/100ths of second/minute,
	call	packed_bcd		; .: in packed BCD
	out	dx,al			; Port 0x2c6 <- day or
					; Port 0x2c1 <- 100ths of s (BCD) or
					; Port 0x2c3 <- minute (BCD)
	inc	dx
	pop	ax
    dw 0xc48a ; mov al,ah		; .. al <- month or second or hour, in
	call	packed_bcd		; .: packed BCD
	out	dx,al			; Port 0x2c7 <- month or
					; Port 0x2c2 <- seconds (BCD) or
					; Port 0x2c4 <- hour (BCD)
	jcxz	.done
	inc	dx
    dw 0xc92b ; sub cx,cx
	push	si
	jmp	short .set_ast_time
.done:	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
    dw 0xc02b ; sub ax,ax
	iret

; Convert al into an unpacked BCD number in <ah, al>.  If al is greater than
; 99, set <ah, al> <- <9, 9>.  Clobber bh.
unpacked_bcd:
	mov	bh,10
    dw 0xe42a ; sub ah,ah
	cmp	al,99
	jna	.1
	mov	al,99
.1:	div	bh
	xchg	al,ah
	ret

; Convert al into a packed BCD number in al.  If al is greater than 99, set
; al = 0x99.  Clobber bh & ah.
packed_bcd:
	call	unpacked_bcd
	shl	ah,1
	shl	ah,1
	shl	ah,1
	shl	ah,1
    dw 0xc40a ; or al,ah
	ret

; End of area to keep resident in memory.
tsr_end:

; Scratch buffer for storing the date & time read from the AST clock.  The
; clock's output is converted to binary before being stored here.
buf:	db	0			; 1/100 second
	db	0			; second
	db	0			; minute
	db	0			; hour
	db	0			; ?
.day:	db	0			; day
	db	0			; month
	db	0			; year - 1980
	db	0			; ?
	db	0			; ?

; Common code to point si & di at the scratch buffer, & set dx to refer to
; the first data (?) port.
get_buf_and_data_port:
	mov	si,buf
    dw 0xfe8b ; mov di,si
	cld
	lea	dx,[bp+1]
	ret

; Load a date or time from the scratch buffer as pointed by si.  Advance si.
load_date_or_time_from_buf:
	lodsw
	xchg	ax,dx
	lodsw
	xchg	ax,cx
	ret

; Output a message.
put_msg:
	mov	ah,9
	int	0x21
	ret

; Read 4 binary bytes from si, advancing si (backwards). Convert them into
; ASCII bytes at di.  Use al as the delimiter (':' or '-').
format_as_ascii:
	xchg	ax,bx
	mov	cx,4
.1:	lodsb
	call	unpacked_bcd
	add	ax,'0'<<8|'0'
	xchg	al,ah
	call	store_char
	xchg	al,ah
	call	store_char
    dw 0xc38a ; mov al,bl
	call	store_char
	loop	.1
	mov	ax,LF<<8|CR
	ret

; Store a character at di, & advance di.  (stosb does not work here as the
; direction may be backwards.)
store_char:
	mov	[di],al
	inc	di
	ret

msg_credits:
	db	"AST RESEARCH, INC. "
	db	"REAL TIME CLOCK INTERFACE PROGRAM "
	db	"for the IBM PERSONAL COMPUTER. "
	db	"(C)Copyright AST RESEARCH, INC., 1982. "
	db	"Licensed material - program property of AST RESEARCH, INC. "
	db	"author - DARYL WATTON"

msg_time:
	db	"Current time is "
.1:	db	"00:00:00"
.2:	db	".00 ",CR,LF,"$"

msg_date:
	db	"Current date is "
.1:	db	"00/00/80"
.2:	db	"/00",CR,LF,"$"

msg_already:
	db	"Real-time clock module is already resident...",CR,LF
	db	"   duplicate request ignored.",CR,LF,"$"

msg_loaded:
	db	CR,LF," resident DATE/TIME processors loaded",CR,LF,CR,LF,"$"

main:	mov	bp,AST_BASE_PORT
	mov	sp,start
	mov	si,new_int_0x21		; Try to check if this program is
    dw 0xfe8b ; mov di,si		; already installed --- see if the
    dw 0xc02b ; sub ax,ax		; current int 0x21's code segment
	push	es			; contains the same code as we do
	mov	es,ax
	mov	ax,[es:0x21*4+2]
	mov	es,ax
	mov	cx,new_int_0x21.done-new_int_0x21
	repz cmpsb
	pop	es
	jnz	.not_instd
	mov	dx,msg_already
	call	put_msg
	jmp	exit
.not_instd:
	lea	dx,[bp+0xb]		; Input from port 0x2cb --- check if
	in	al,dx			; AST clock is enabled (?)
	cmp	al,0xde
	jz	.ast_enabled
	mov	al,0xff			; If not, enable it... (?)
	push	dx
	lea	dx,[bp+0x12]
	out	dx,al			; Port 0x2d2 <- 0xff
	inc	dx
	out	dx,al			; Port 0x2d3 <- 0xff
	pop	dx
	mov	al,0xde
	out	dx,al			; Port 0x2cb <- 0xde
.ast_enabled:
	mov	cx,8			; Try to retrieve the AST clock's
					; idea of the current date & time;
					; retry 8 times
.ast_retry_read_date_time:
	push	cx
	mov	cl,7
	call	get_buf_and_data_port	; Set (si &) di to point to our
					; scratch buffer, & dx to 0x2c1
					; (AST_BASE_PORT + 1)
.ast_read_date_time_loop:
	in	al,dx			; Read a byte from port 0x2c1/0x2c2/
	inc	dx			; 0x2c3/0x2c4/0x2c5/0x2c6/0x2c7
	push	ax			; ..
	push	cx			;  :
	mov	cl,4			;  :
	shr	al,cl			;  : convert al from packed BCD
	pop	cx			;  : to binary
	pop	bx			;  :
	mov	bh,10			;  :
	mul	bh			;  :
	and	bl,0x0f			;  :
    dw 0xc302 ; add al,bl		; .:
	stosb				; Store al in the scratch buffer
	loop	.ast_read_date_time_loop
	lea	dx,[bp+0x14]		; Read from port 0x2d4 to see if the
	in	al,dx			; read contents are sane (?); if
	test	al,al			; not, retry
	pop	cx
	loopne	.ast_retry_read_date_time
    dw 0xdb2b ; sub bx,bx
	lea	dx,[bp+9]		; Check whether the BCD month (via
	in	al,dx			; port 0x2c7) wrapped around & we
	cmp	al,[di-1]		; need to update the year: to do
					; this, read the binary month (port
					; 0x2c9) (?)
	jna	.ast_update_year_month
	inc	bx
.ast_update_year_month:
	mov	al,[di-1]
	out	dx,al			; Port 0x2c9 <- updated month (binary)
	inc	dx
	in	al,dx
    dw 0xc302 ; add al,bl
	out	dx,al			; Port 0x2ca <- updated {year
					; - 1980} (binary)
	stosw				; Store the {year - 1980} (should
					; this be stosb?)
	call	get_buf_and_data_port	; Set BIOS & DOS's idea of the time
	call	load_date_or_time_from_buf
	mov	ah,0x2d
	int	0x21
	inc	si			; Set DOS's idea of the date
	call	load_date_or_time_from_buf
    dw 0xed2a ; sub ch,ch
	add	cx,1980
	mov	ah,0x2b
	int	0x21
	call	get_buf_and_data_port	; Format the time as an ASCII string
	mov	al,':'
	mov	di,msg_time.1
	add	si,byte 3
	std
	call	format_as_ascii
	mov	byte [di-1],' '		; Neuter the bogus trailing ':'
	mov	si,buf.day		; Format the date as an ASCII string,
	cld				; (in mm/dd/yy order...)
	mov	ax,[si]
	xchg	al,ah
	mov	[si],ax
	mov	al,[si+2]
	add	al,80
	mov	[si+2],al
	mov	al,'/'
	mov	di,msg_date.1
	call	format_as_ascii
	mov	[msg_date.2],ax		; Do some final corrections to the
	mov	byte [msg_date.2+2],'$'	; formatting
	mov	byte [msg_time.2],'.'
	mov	dx,msg_loaded		; Display everything
	call	put_msg
	mov	dx,msg_date
	call	put_msg
	mov	dx,msg_time
	call	put_msg
    dw 0xc02b ; sub ax,ax		; Stuff a zero flags register value
	mov	[zero_flags],ax		; in the PSP (but why...?)
	mov	es,ax			; Now hook up our new int 0x21
	mov	ax,new_int_0x21		; handler, while storing the old int
	xchg	ax,[es:0x21*4]		; handler pointer
	mov	[org_int_0x21],ax
	mov	ax,cs
	xchg	ax,[es:0x21*4+2]
	mov	[org_int_0x21+2],ax
	push	ds			; Now quit & make ourselves resident
	pop	es
	mov	dx,tsr_end
	int	0x27
