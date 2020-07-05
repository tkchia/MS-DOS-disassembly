#!/bin/sh
#
# Preprocess and assemble ASM.ASM, HEX2BIN.ASM, or TRANS.ASM from
# Microsoft's MS-DOS v1.25 source code release, using the Netwide Assembler
# (nasm).  This makefile requires GNU make.
#
#	Copyright (c) 2020 TK Chia.
#
#	Permission is hereby granted, free of charge, to any person
#	obtaining a copy of this software and associated documentation files
#	(the "Software"), to deal in the Software without restriction,
#	including without limitation the rights to use, copy, modify, merge,
#	publish, distribute, sublicense, and/or sell copies of the Software,
#	and to permit persons to whom the Software is furnished to do so,
#	subject to the following conditions:
#
#	The above copyright notice and this permission notice shall be
#	included in all copies or substantial portions of the Software.
#
#	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#	NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#	BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
#	ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#	CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#	SOFTWARE.

vpath %.ASM ../source

ASM = nasm -w+orphan-labels -w-macro-params -w-label-redef-late -w-other \
	   -P nasmify.inc

default all: ASM.COM HEX2BIN.COM TRANS.COM

clean:
	$(RM) *.COM *.LST *.PRN *.HEX *~

ASM.COM: ASM.ASM nasmify.inc
	$(ASM) -l $(@:.COM=.LST) -o $@ $<

HEX2BIN.COM: HEX2BIN.ASM nasmify.inc
	$(ASM) -l $(@:.COM=.LST) -o $@ $<

TRANS.COM: TRANS.ASM nasmify.inc
	$(ASM) -l $(@:.COM=.LST) -o $@ $<

.PHONY: default all clean
