; MS-DOS v1.25 SYS.COM
; SHA-256: eaf5e7dbd3edbef716765ffb0a7554351061a6e80af5ae4cec5707d76cb7d7e1
; Binary is part of MS-DOS v1.25 code as released by MS under MIT License
; Use the Netwide Assembler (nasm) to reproduce the original program

; This program updates the system files ibmbio.com and ibmdos.com on a
; target disk, using files from the disk in the current drive.  The program
; will likely be invoked in one of these ways:
;	sys a:
;	sys b:

	bits	16

	org	0x0100

LF		equ	10
CR		equ	13
A_HIDDEN	equ	0x02
A_SYSTEM	equ	0x04
fcb1		equ	$-0x0100+0x5c	; MS-DOS parses the command line
					; arguments & lays out two File
					; Control Block (FCB) structures at
					; [0x5c] & [0x6c] in the Program
					; Segment Prefix (PSP)

struc	Fcb				; File Control Block (FCB) structure:
.drv:	resb	1			; - drive number
.fn:	resb	8			; - filename
.ext:	resb	3			; - extension
.blk:	resw	1			; - current block number
.rsz:	resw	1			; - logical record size
.fsz:	resd	1			; - file size
.mdate:	resw	1			; - last write date
.mtime:	resw	1			; - last write time
	resb	8			; - reserved
.blkrec:resb	1			; - record within current block
.ranrec:resd	1			; - random access record number
endstruc

struc	Xfcb				; Extended FCB (XFCB) structure:
.sig:	resb	1			; - 0xff signature
	resb	5			; - reserved
.attr:	resb	1			; - file attribute
.fcb:	resb	Fcb_size		; - standard FCB
endstruc

struc	File				; Internal file structure used by
					; this program:
.xfcb:	resb	Xfcb_size		; - XFCB
.srcmdt:resw	1			; - last write date for source file
					;   (if writing target file)
.srcmtm:resw	1			; - last write time for source file
					;   (if writing target file)
.rdrecs:resw	1			; - no. of records (bytes) actually
					;   read from source file
.drv		equ	.xfcb+Xfcb.fcb+Fcb.drv
.fsz		equ	.xfcb+Xfcb.fcb+Fcb.fsz
.rsz		equ	.xfcb+Xfcb.fcb+Fcb.rsz
.mdate		equ	.xfcb+Xfcb.fcb+Fcb.mdate
.mtime		equ	.xfcb+Xfcb.fcb+Fcb.mtime
.ranrec		equ	.xfcb+Xfcb.fcb+Fcb.ranrec
endstruc

IBMBIO_MAX_SZ	equ	0x8000		; Maximum number of bytes allowed
					; in ibmbio.com
IBMDOS_MAX_SZ	equ	0x2000		; Maximum number of bytes allowed
					; in ibmdos.com

start:	jmp	short main

err_inval_parm:
	mov	dx,msg_inval_parm
	jmp	exit_msg

err_inval_drv:
	mov	dx,msg_inval_drv
	jmp	exit_msg

; Ask the user to insert a system disk in the current drive.  Then try
; reading it (again) for system files.
ask_insert_disk:
	mov	al,[cur_drv]		; Plug the current drive into our
	add	al,'A'-1		; message
	mov	[msg_insert_disk.1],al
	mov	dx,msg_insert_disk	; Display the message
	mov	ah,9
	int	0x21
	mov	ax,0x0c08		; Flush keyboard buffers & wait for
	int	0x21			; keystroke
    dw 0xc032 ; xor al,al		; Set al <- 0 to indicate to `main'
	; fall through			; (again) that first argument has
					; valid drive letter

main:	cmp	byte [fcb1+Fcb.fn],' '	; Test if the first argument has a
	jnz	err_inval_parm		; file name --- this is invalid
	cmp	al,0xff			; Test if first argument has bad
	jz	err_inval_drv		; drive letter --- this is invalid
					; (al = 0xff on program entry, if
					; first FCB does not have valid
					; drive letter)
	cmp	byte [fcb1+Fcb.drv],0	; Test if there is a first argument
	jz	err_inval_drv		; --- there must be one!
	mov	ah,0x19			; Get the current (source) drive
	int	0x21
	inc	al			; Make it 1-based
	mov	[cur_drv],al
	cmp	[fcb1+Fcb.drv],al	; Make sure source drive != target
	jz	err_inval_drv		; drive!
	mov	ah,0xf			; Open ibmbio.com on source drive
	mov	dx,ibmbio
	int	0x21
    dw 0xc00a ; or al,al		; If this fails, ask user to insert
	jnz	ask_insert_disk		; a system disk in source drive,
					; then retry everything
	mov	dx,ibmdos		; Open ibmdos.com on source drive
	mov	ah,0xf
	int	0x21
    dw 0xc00a ; or al,al		; If this fails, also ask user to
	jnz	ask_insert_disk		; insert system disk & retry
	mov	ah,0x1a			; Set the Disk Transfer Area (DTA)
	mov	dx,ibmbio_buf		; for reading ibmbio.com
	int	0x21
	mov	ax,1			; Set the logical record size to 1
	mov	[ibmbio+File.rsz],ax	; byte for both ibmbio.com & ibmdos.com
	mov	[ibmdos+File.rsz],ax	; FCBs
	mov	bx,ibmbio		; Read ibmbio.com into memory; this
	mov	cx,IBMBIO_MAX_SZ	; should be at most 0x8000 bytes
	call	read_file
	mov	dx,ibmdos_buf		; Set the DTA for reading ibmdos.com
	mov	ah,0x1a
	int	0x21
	mov	bx,ibmdos		; Read ibmdos.com into memory; this
	mov	cx,IBMDOS_MAX_SZ	; should be at most 0x2000 bytes
	call	read_file
	mov	al,[fcb1]		; Now plug the destination drive
	mov	[ibmdos+File.drv],al	; number into the ibmbio.com &
	mov	[ibmbio+File.drv],al	; ibmdos.com FCBs
	mov	dl,[fcb1+Fcb.drv]	; Get the cluster size in bytes for
	mov	ah,0x1c			; the target drive
	int	0x21
	push	cs
	pop	ds
	mov	ah,0
	mul	cx
	xchg	ax,cx
	mov	bx,ibmbio		; Make sure the target drive's
	call	compare_sizes		; ibmbio.com occupies the same number
	jnz	err_incompat		; of clusters as the source's
	mov	bx,ibmdos		; Make sure the target drive's
	call	compare_sizes		; ibmdos.com is at least as large
	ja	err_incompat		; as the source's
	mov	dx,ibmbio_buf		; Set the DTA for writing ibmbio.com
	mov	ah,0x1a
	int	0x21
	mov	bx,ibmbio		; Write out ibmbio.com
	call	write_file
	mov	dx,ibmdos_buf		; Set the DTA for writing ibmdos.com
	mov	ah,0x1a
	int	0x21
	mov	bx,ibmdos		; Write out ibmdos.com
	call	write_file
	mov	dx,msg_success		; We are done!
	; fall through

; Display a message & exit.
exit_msg:
	mov	ah,9
	int	0x21
	int	0x20

err_no_room:
	mov	dx,msg_no_room
	jmp	short exit_msg

err_incompat:
	mov	dx,msg_incompat
	jmp	short exit_msg

; Read a file given by the `File' structure at [bx] into the current Disk
; Transfer Area (DTA), & record information about the source file.  cx gives
; the maximum number of records (well, bytes) to read.
read_file:
	mov	ah,0x27			; Do a random access FCB read
    dw 0xd38b ; mov dx,bx
	int	0x21
	mov	[bx+File.rdrecs],cx	; Record the source file's size &
	mov	ax,[bx+File.mdate]	; last modified date & time
	mov	[bx+File.srcmdt],ax
	mov	ax,[bx+File.mtime]
	mov	[bx+File.srcmtm],ax
	ret

; Given a `File' structure [bx] where the source file information has been
; filled in, open the target file, & compare the source file's cluster count
; to the target file's.  cx should contain the number of bytes per cluster
; for the target disk.
;
; Leave the comparison results in the flags register.
compare_sizes:
	mov	ah,0xf			; Open the target file
    dw 0xd38b ; mov dx,bx
	int	0x21
    dw 0xc00a ; or al,al		; If the file cannot be opened, exit
	jnz	err_no_room		; immediately
	mov	ax,[bx+File.fsz]	; Compute the number of clusters in
    dw 0xd233 ; xor dx,dx		; the target file --- divide the
    dw 0xc103 ; add ax,cx		; file size by the cluster size &
	dec	ax			; round it up
	div	cx
	push	ax
	mov	ax,[bx+File.rdrecs]	; Compute the number of clusters in
    dw 0xc103 ; add ax,cx		; the source file
	dec	ax
    dw 0xd233 ; xor dx,dx
	div	cx
	pop	dx
    dw 0xc23b ; cmp ax,dx		; Compare
	ret

; Write a file given by the `File' structure at [bx].  The contents to write
; will come from the current Disk Transfer Area (DTA).
write_file:
    dw 0xd38b ; mov dx,bx
    dw 0xc033 ; xor ax,ax		; Set the random access record
	mov	[bx+File.ranrec],ax	; number to 0
	mov	[bx+File.ranrec+2],ax
	inc	ax			; Set the logical record size to 1
	mov	[bx+File.rsz],ax
	mov	ah,0x28			; Do a random access write --- write
	mov	cx,[bx+File.rdrecs]	; the same number of bytes (records)
	int	0x21			; we read earlier from the source
	mov	ax,[bx+File.srcmdt]	; Make sure the file's date & time
	mov	[bx+File.mdate],ax	; are the same as the original's
	mov	ax,[bx+File.srcmtm]
	mov	[bx+File.mtime],ax
	mov	ah,0x10			; Close the target file, flush it
	int	0x21			; out to disk
	ret

msg_inval_drv:
	db	"Invalid drive specification$"

msg_inval_parm:
	db	"Invalid parameter$"

msg_insert_disk:
	db	"Insert system disk in drive "
.1:	db	"A",CR,LF
	db	"and strike any key when ready",CR,LF,"$"

msg_no_room:
	db	"No room for system on destination disk$"

msg_incompat:
	db	"Incompatible system size$"

msg_success:
	db	"System transferred$"

cur_drv:
	db	0

; File structure for ibmbio.com.
ibmbio:	istruc	File
    at File.xfcb,istruc Xfcb
	db	0xff,0,0,0,0,0
	db	A_HIDDEN|A_SYSTEM
	db	0
	db	"IBMBIO  COM"
	iend
	dw	0,0,0
	iend

; File structure for ibmdos.com.
ibmdos:	istruc	File
    at File.xfcb,istruc Xfcb
	db	0xff,0,0,0,0,0
	db	A_HIDDEN|A_SYSTEM
	db	0
	db	"IBMDOS  COM"
	iend
	dw	0,0,0
	iend

section	.bss	align=2

; Where to read the contents of ibmbio.com & ibmdos.com.
ibmdos_buf:
	resb	IBMDOS_MAX_SZ
ibmbio_buf:
