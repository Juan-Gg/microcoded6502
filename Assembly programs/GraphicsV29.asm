; Microcoded 6502 computer "Kernel"
; Includes I/O routines, a simple monitor and EhBASIC support.

; By JuanGg on June 2020
; https://juangg-projects.blogspot.com/
; http://forum.6502.org/viewtopic.php?f=12&t=5811

; To assemble with as65: http://web.archive.org/web/20190301123637/http://www.kingswood-consulting.co.uk/assemblers/
; Still needs documentation. Keep in mind that I have no idea of what I'm doing.


;Preliminary monitor routines
;Cursor needs fixing
;Working simple monitor with Examine, Write and Jump comands.
;Lets try running TinyBasic. Now that hardware is fixed, it runs without issue.
;Removed TinyBasic support, now runs EhBASIC (Only character input/output)
;Added more symbols to the keyboard subroutine.

;LCD addreses:  E1     E2
;              $9C02  $9C04 for instructions
;              $9C03  $9C05 for data


;                            RS RW  0  1  2  3  4  5  6  7
;Set DDRAM address:          0  0   1  AD AD AD AD AD AD AD
;Read busy flag & address:   0  1   BF AC AC AC AC AC AC AC
;Write data to CG or DDRAM:  1  0	-- -- -- -- -- -- -- --
;Read data from CG or DDRAM: 1  1   -- -- -- -- -- -- -- --

;DDRAM addresses: 	First line:	 $00-$27
;				  	Second line: $40-$67

;KEYBOARD:
; $9C08

;LCD
lcd10 		equ	$9C02  
lcd11		equ	$9C03 
lcd20 		equ	$9C04  
lcd21		equ	$9C05 

;Keyboard
kbrd		equ $9C08

;VGA
vRAMbase	equ $7000
vgaColor    equ $9C10

;UART
uartData	equ $9000
uartStatus	equ $9001

;MONITOR
monBuffer	equ $6f00 ;$6f00...$6fff

;EhBASIC
warmStart	equ $0000
coldStart	equ $c000

;HOUSEKEEPING

			bss ;Uninitialized data segment
			org $14 ;...$5A free zero page locations not used by EhBASIC
			data	;Initialized data (e.g strings in ROM)
			org $ff00
			code ;Code segment. This is where the monitor and I/O routines live.
			org $f000

;************************************************************
;*						 MACROS 							*
;************************************************************
printMacro	macro text

			data
string\?	db text,0
			
			code

			lda #hi string\?
			sta strAddr+1
			
			lda #lo string\?
			sta strAddr
			
			jsr printStr
			
			endm
			
;************************************************************
;*				 ZERO-PAGE VARIABLES 						*
;************************************************************
			bss
kbBuffer    ds	1 ;
kbFlags		ds	1 ;

;SCREEN STUFF:

textHIdx	ds	1 ;
textVIdx	ds	1 ;
vRAMIdx		ds	2
vRAMIdx2	ds	2
cROMIdx		ds	2

;Misc
strAddr		ds	2
char		ds	1 ;To be printed by vgaCharB

;Monitor
parsedHex	ds 	2
monAddr1	ds	2
monAddr2	ds	2 ;

;Graphics
gX0			ds	1
gY0			ds	1
gX1			ds	1
gY1			ds	1
gColor 		ds	1 ; Bit 7: 0 background, 1 foreground

;UART
uInt		ds  1 ; 
uVgaDis		ds	1 ; Bit 7: 0 enabled, 1 disabled (for speed)

;Misc variables for use in subroutines
tmp0		ds	1 ;
tmp1		ds 	1 ;
tmp2 		ds	1 ;
tmp3		ds 	1 ;
tmp4		ds	1 ;
tmp5		ds	1 ;
tmp6		ds	1 ;
tmp7		ds 	1 ;
tmp8		ds	1 ;
tmp9		ds	1 ;

			code

;************************************************************
;*						 "RESET"							*
;************************************************************	
			;RESET
			
RESET:		;Reset stack pointer
			ldx #$ff
			txs
			
			;Reset keyboard
			lda #%00001000 ;Caps lock set by default
			sta kbFlags
			lda #$0
			sta kbBuffer
			
			;Reset video
			lda #$0
			sta textHIdx
			sta textVIdx
			
			jsr vgaCls
			lda #$47 ;Default color 
			sta vgaColor
			
			;Reset graphics
			lda #$80
			sta gColor ; Bit 7: 0 background, 1 foreground
			
			;UART
			;Flags to check during interrupts.
			lda #%00000000 ;(None)
			sta uInt
			
			;Enable interrupts on byte received/sent.(None)
			lda #%00000000
			sta uartStatus
			
			;Enable vgaTerminal, UART is always enabled.
			lda #0
			sta uVgaDis
			
			;Enable interrupts
			cli
			
			printMacro	"\n\rROM MONITOR V1.0"
			jsr shCursor
			jmp monitor
			
			
;************************************************************
;*					 UART ROUTINES 							*
;************************************************************
uartHadler:	;Nothing to see here (yet).
			rts

			;Waits until tx ready, then send byte.
uartSend:	pha
usWait		lda uartStatus
			and #%00000010
			beq usWait
			pla
			sta uartData
			rts
				
;************************************************************
;*					 GRAPHICS ROUTINES 						*
;************************************************************		
;###############DRAW LINE BETWEEN TWO POINTS#################	
;Based on https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
;Between (gX0, gY0) and (gX1, gY1)

;XR : x
;YR : y

;Variables:
			;tmp1 is used by gPlotPix
gdX			equ tmp2 ;2 bytes
gdY			equ tmp4 ;2 bytes
gD			equ tmp6 ;2 bytes
gInc		equ tmp8 ;1 byte

gLine:		pha
			tya
			pha
			txa
			pha

		;if abs(y1-y0)<abs(x1-x0)
			sec
			lda gX1
			sbc	gX0
			bcs gLXPos 
			
			eor #$ff
			adc #1 ;c is clear
gLXPos:		sta gdX
		
			sec
			lda gY1
			sbc	gY0
			bcs gLYPos 
			
			eor #$ff
			adc #1 ;c is clear
gLYPos:		
			cmp gdX
			
			bcs gLCheckY ;abs(y1-y0)>=abs(x1-x0)
			
			;if x0 > x1
			lda gX1
			cmp gX0
			bcs gLX0smaller
			
			;plotLineLow(x1, y1, x0, y0)
			
			ldx gX0
			ldy gX1
			stx gX1
			sty gX0
			
			ldx gY0
			ldy gY1
			stx gY1
			sty gY0
			
			
			jsr gLineInit
			jsr gLineLow
			jmp gLineEnd
			
gLX0smaller	;plotLineLow(x0, y0, x1, y1)
			jsr gLineInit
			jsr gLineLow
			jmp gLineEnd
			
gLCheckY;else	
			;if y0 > y1
			lda gY1
			cmp gY0
			bcs gLY0smaller
		
			;plotLineHigh(x1, y1, x0, y0)
			
			ldx gX0
			ldy gX1
			stx gX1
			sty gX0
			
			ldx gY0
			ldy gY1
			stx gY1
			sty gY0
			
			jsr gLineInit
			jsr gLineHigh
			jmp gLineEnd
			
gLY0smaller	;plotLineHigh(x0, y0, x1, y1)
			jsr gLineInit
			jsr gLineHigh
			jmp gLineEnd
			
gLineEnd	pla
			tax
			pla
			tay
			pla
			rts
			



;This part is the same for lineHigh or lineLow
gLineInit:	pha
			;dx = x1-x0
			;gdX is a 2 byte integer
			sec
			lda gX1
			sbc gX0
			sta gdX
			lda #0 ;gX1 + 1
			sbc #0 ;gX0 + 1
			sta gdX + 1
			
			;dy = y1 - y0
			;gdY is a 2 byte integer
			sec
			lda gY1
			sbc gY0
			sta gdY 
			lda #0 ;gY1 + 1
			sbc #0 ;gY0 + 1
			sta gdY + 1
			
			;yi = 1 (low) or xi = 1 (high)
			lda #1
			sta gInc 
			
			pla
			rts
				


gLineLow:	
			asl gdY ;dy->2*dy. We don't ever need dy, but 2*dy
			rol gdY + 1
			
		;if dy < 0
			lda gdY + 1
			bpl gLdYPos
			
			;yi = -1 <(low) or xi = -1 (high)>
			lda #$ff
			sta gInc
			
			;dy = -dy
			lda gdY
			eor #$ff
			sta gdY
			lda gdY + 1
			eor #$ff
			sta gdY + 1
			
			inc gdY
			bne gLdYPos ; Skip
			inc gdY + 1			
gLdYPos:
		;end if
		
			;D = 2*dy - dx
			;lo gdY is in a (2*dy)
			sec
			sbc gdX
			sta gD
			lda gdY + 1
			sbc gdX + 1
			sta gD +1
			
			;We no longer need dx, but 2*dx
			asl gdX
			rol gdX + 1
		
			;y = y0
			ldy gY0
			
		;for x from x0 to x1
			ldx gX0
			
gLLineLoop:	;plot(x,y)
			stx gX0
			sty gY0
			jsr gPlotPix
			
			;if D>(=)0 Does not matter much if D=0
			lda gD + 1 ;Only need to check hi byte
			bmi gLDNegEq
			
			;y = y + yi
			tya 
			clc
			adc gInc
			tay
			
			;D = D - 2*dX
			sec
			lda gD
			sbc gdX
			sta gD
			lda gD + 1
			sbc gdX + 1
			sta gD + 1
			;end if
			
gLDNegEq:			
			;D = D +2*dy
			clc
			lda gD
			adc gdY
			sta gD
			lda gD + 1
			adc gdY + 1
			sta gD + 1
			
		;end for
			inx
			cpx gX1
			bcc gLLineLoop
			beq gLLineLoop
			
			rts
			
			
gLineHigh:	
			asl gdX ;dx->2*dx. We don't ever need dy, but 2*dx
			rol gdX + 1
			
		;if dx < 0
			lda gdX + 1
			bpl gHdXPos
			
			;xi = -1 
			lda #$ff
			sta gInc
			
			;dx = -dx
			lda gdX
			eor #$ff
			sta gdX
			lda gdX + 1
			eor #$ff
			sta gdX + 1
			
			inc gdX
			bne gHdXPos ; Skip
			inc gdX + 1			
gHdXPos:
		;end if
		
			;D = 2*dx - dy
			;lo gdX is in a (2*dx)
			sec
			sbc gdY
			sta gD
			lda gdX + 1
			sbc gdY + 1
			sta gD +1
			
			;We no longer need dy, but 2*dy
			asl gdY
			rol gdY + 1
			
			
			;x = x0
			ldx gX0
			
		;for y from y0 to y1
			ldy gY0
			
gHLineLoop:	;plot(x,y)
			stx gX0
			sty gY0
			jsr gPlotPix			
			
			;if D>(=)0 Does not matter much if D=0
			lda gD + 1 ;Only need to check hi byte
			bmi gHDNegEq
			
			;x = x + xi
			txa 
			clc
			adc gInc
			tax
			
			;D = D - 2*dY
			sec
			lda gD
			sbc gdY
			sta gD
			lda gD + 1
			sbc gdY + 1
			sta gD + 1
			;end if
			
gHDNegEq:			
			;D = D +2*dx
			clc
			lda gD
			adc gdX
			sta gD
			lda gD + 1
			adc gdX + 1
			sta gD + 1
			
		;end for
			iny
			cpy gY1
			bcc gHLineLoop
			beq gHLineLoop

			rts
			
			
;##################PLOT PIXEL AT gX0,gY0##################
gPlotPix:	pha
			tya
			pha
			
			;Clear hi vRAMIdx
			lda #0
			sta vRAMIdx + 1
			
			;Multiply gY0 by 32 to get horizontal line.
			;Each horizontal line is 32 bytes, of which 25 are shown.
			;Add vRAMbase and store in vRAMIdx
			
			lda gY0
			ldy #5
gpMult:		asl a
			rol vRAMIdx + 1
			dey
			bne gpMult
			
			sta vRAMIdx
			
			clc
			lda vRAMIdx + 1
			adc #hi vRAMbase
			sta vRAMIdx + 1
			
			;Now we just have to add gX0
			;Save pixel index within byte in tmp1
			lda gX0
			and #%00000111
			sta tmp1
			;Divide by 8 pixels/byte
			lda gX0
			lsr a
			lsr a
			lsr a
			
			;Add it to vRAMIdx. No need to carry into upper byte,
			;gX0 would be out of range for carry to be generated.
			clc 
			adc vRAMIdx
			sta vRAMIdx
			
			;Generate pixel mask from index within byte in tmp1
			sec
			lda #0
gpPixMask:	ror a
			dec tmp1
			bpl gpPixMask
		
			sta tmp1 ;Now tmp1 holds pixel mask
			
			;Plot pixel (y is 0)
			; Bit 7: 0 background, 1 foreground
			bit gColor
			bmi gpPixFor
			
			;tmp1 is in a 
			eor #$ff
			sta tmp1
			
			lda (vRAMIdx),y
			and tmp1
			sta (vRAMIdx),y
			
			jmp gpPixEnd
			
gpPixFor:	lda (vRAMIdx),y
			ora tmp1
			sta (vRAMIdx),y
			
gpPixEnd:	pla
			tay
			pla
			rts

;************************************************************
;*					 EH BASIC ROUTINES 						*
;************************************************************	
;###############START######################
basicStart:	lda #lo basicIn	 ;INPUT VECTOR
			sta $205
			lda #hi basicIn
			sta $206
			
			lda #lo basicOut ;OUTPUT VECTOR	
			sta $207
			lda #hi basicOut
			sta $208
			
			ldx #$ff ;RESET STACK
			txs
			
			printMacro "\n\rEhBASIC [C]old/[W]arm?"
			
bSKb:		jsr basicIn ;Substituted the code below for this, adding uart support.
			bcc bSKb

			;lda kbBuffer
			; beq bSKb
			; ldy #$0
			; sty kbBuffer
			
			cmp #'W'
			beq doWarm
			
			cmp #'C'
			bne	basicStart
			
			jmp coldStart
			
doWarm:		jmp warmStart
			
;###############INPUT#####################			
basicIn:	lda kbBuffer
			beq bINoKey ;No key
			
			pha	;Clear buffer
			lda #0
			sta kbBuffer
			pla
			
			sec 
			rts
			
bINoKey:	lda uartStatus
			and #%00000001
			beq bINoChar
			lda uartData
			
			sec
			rts
			
bINoChar:	lda #0
			clc
			rts


;############OUTPUT#######################
basicOut:	jsr shCursor
			jsr vgaTerm ;Pla affects N and Z
			jsr shCursor
			rts
			
			
;************************************************************
;*						 MONITOR 							*
;************************************************************
brkMonitor:	pha
			txa
			pha
			tya
			pha
			
			printMacro "\n\r PC  AC XR YR SR\n\r"
			
			tsx		   
			; Get hi PC from stack
			lda $106,x ; 100 + 3(3 x pha) + 3
			jsr mPrByte
			; Get lo PC from stack
			lda $105,x ; 100 + 3(3 x pha) + 2
			jsr mPrByte
			
			lda #' '
			jsr vgaTerm
			
			; Get AC from stack
			lda $103,x ; 100 + 2(2 x pha) + 1
			jsr mPrByte
			
			lda #' '
			jsr vgaTerm
			
			; Get XR from stack
			lda $102,x ; 100 + 1(1 x pha) + 1
			jsr mPrByte
			
			lda #' '
			jsr vgaTerm
			
			; Get YR from stack
			lda $101,x ; 100 + 1
			jsr mPrByte
			
			lda #' '
			jsr vgaTerm
			
			; Get SR from stack
			lda $104,x ; 100 + 3(3 x pha) + 1
			jsr mPrByte
			
			jsr shCursor


monitor:	jsr shCursor
			lda #'\n'
			jsr vgaTerm
			lda #'\r'
			jsr vgaTerm
			lda #'@'
			jsr vgaTerm
			jsr shCursor
			
			
		
			;Read from keyboard, reset keyboard buffer (one byte)
			;Print input to terminal and save in monBuffer.
			;When '\r', parse it.
			ldx #0; Buffer index
			
			
; mkLoop		lda kbBuffer 
			; beq mkLoop	;If no character, loop
			; ldy #$0			
			; sty kbBuffer ;Reset buffer
			
mkLoop		jsr basicIn ;Substituted the above for this, ading uart support.
			bcc mkLoop
			
			cmp #'\b'	;Handle backspace
			bne mkLnoBS	;No backspace
			cpx #0		;if buffer index is 0, don't do anything
			beq mkLoop
			dex			;otherwise dec index and print '\b'
			
			jsr shCursor
			jsr vgaTerm	;Print backspace
			jsr shCursor
			
			jmp mkLoop
			
mkLnoBS		sta monBuffer, x			
			cmp #'\r'
			beq mParse
			
			jsr shCursor
			jsr vgaTerm	;Print character
			jsr shCursor
			
			inx
			bne mkLoop
			
			;Comands
			;E( )<addr1>(.<addr2>)Examine memory
			;W( )<addr1>:<byte> <byte> ... Write to memory
			;J( )<addr> Jump to a memory location
			;IA000.A100 Interpret memory
			
			
mParse		
			ldx #0
			lda monBuffer, x
			inx
			
			cmp #'E'
			beq mExamine
			
			cmp #'W'
			beq mWrite
			
			cmp #'J'
			beq mJump
			
			cmp #'B'
			beq goBasic
			
			cmp #'R'
			beq mReturn
			
			cmp #'I'
			beq mExamine ;Examine decides.
			
			cmp #'S'
			beq mSubr
			
			jmp monitor
			
;###############BASIC####################
			
goBasic 	jmp basicStart
			
			
;################RETURN##################
;Goes back after a brk
mReturn		pla
			tay
			pla
			tax
			pla
			rti

;#################JUMP###################	
;Parse and execute Jump comand J( )<addr>
			
mJump		
			ldy #0 ; Address offset
			;Ignore space after comand
			lda monBuffer, x ;x is 1
			cmp #' '
			bne mJNoSpace
			inx ;monBuffer index
mJNoSpace	jsr mParseHex

			jmp (parsedHex) ;Jump to requested address
			
;#################SUBROUTINE###################	
;Parse and execute Subroutine comand S( )<addr>
mSubr:		ldy #0 ; Address offset
			;Ignore space after comand
			lda monBuffer, x ;x is 1
			cmp #' '
			bne mSNoSpace
			inx ;monBuffer index
mSNoSpace	jsr mParseHex

			jsr mSJSR
			jmp monitor
mSJSR		jmp (parsedHex) ;Jump to requested address

;#################WRITE###################	
;Parse and execute Write comand W( )<addr1>:<byte> <byte> ...	
mWrite		
			ldy #0 ; Address offset
			;Ignore space after comand
			lda monBuffer, x ;x is 1
			cmp #' '
			bne mWNoSpace
			inx ;monBuffer index
mWNoSpace	jsr mParseHex

			;Save starting address in addr1
			lda parsedHex
			sta monAddr1
			lda parsedHex+1
			sta monAddr1+1
			
			lda monBuffer, x
			cmp #':'
			bne mWEnd ;No colon, we are done
			;Read data byte
mWRdByte	inx	;Buffer index
			jsr mParseHex	;Parse byte
			lda parsedHex  
			sta (monAddr1), y ;Save it in memory
			iny ;Next memory location
			
			lda monBuffer, x
			cmp #' ' ;If no space separator, we are done.
			bne mWEnd
			beq mWRdByte ;Always taken.
			
mWEnd		
			jmp monitor


;#################EXAMINE###################	
;Parse and execute Examine comand E( )<addr1>(.<addr2>)	
;Parse and execute Interpret comand I( )<addr1>(.<addr2>)	
mExamine	
			;Ignore space after comand
			lda monBuffer, x
			cmp #' '
			bne mENoSpace
			inx 
mENoSpace	jsr mParseHex
			
			;Save starting address in addr1
			lda parsedHex
			sta monAddr1
			lda parsedHex+1
			sta monAddr1+1
			
			;Default for addr2
			lda #0
			sta monAddr2
			sta monAddr2 +1
			
			;If end address given, parse it
			lda monBuffer, x
			cmp #'.'
			bne mENoEnd
			inx
			jsr mParseHex
			
			lda parsedHex
			sta monAddr2
			lda parsedHex+1
			sta monAddr2+1
			
			
mENoEnd		lda monBuffer ;This handles the interpret comand
			cmp #'I'
			beq mInterpret
			
			jsr printMem
			jsr shCursor
			jmp monitor
			
mInterpret	jsr printAscii
			jsr shCursor
			jmp monitor
			
			
;##############PARSE HEX#################
;Takes ascii hex from monBuffer at y, translates into
; binary in parsedHex until non Hex chars are found.
;If longer than 4 chars, last 4 are taken.
mParseHex:	
			;Clear parsedHex
			lda #0
			sta parsedHex
			sta parsedHex+1

mPHloop		lda monBuffer,x ;Read char from buffer
			
			jsr mHexBin ;Translate to binary
			bcs mPHnoHex ;If no hex, we are done
			
			asl a			
			asl a
			asl a
			asl a
			
			asl a
			rol parsedHex
			rol parsedHex+1
			asl a
			rol parsedHex
			rol parsedHex+1
			asl a
			rol parsedHex
			rol parsedHex+1
			asl a
			rol parsedHex
			rol parsedHex+1
			inx
			bne mPHloop ;Next character, unless buffer ends
mPHnoHex			
			rts
			
			
			
;############ASCII HEX (IN A) TO BINARY############			
mHexBin:	sec
			sbc #'0' ; So '0' would be 0
			cmp #10
			bcc mHBNum ;If less than 10, number.
			sbc #'A'-$a-'0' ; So 'A' would be $A
			cmp #$f +1
			bcs mHBOther ;Not in 'A'-'F'
			cmp #$A
			bcc mHBOther ;Not in 'A'-'F'
mHBNum		clc	;Clear carry as flag
			rts	;No problem, return number $0...$f
mHBOther	sec ;Not '0'-'F', set carry as flag
			rts

			
			
;#########print memory from monAddr1 to monAddr2###########
			
			;LSB addr, >=8 ->8, otherwise ->0
			;So everything lines up for several lines of output
			;This is not being used at the moment. It's up to the user
prMemAdj:	lda monAddr1
			and #%00001111
			cmp #8
				
			lda monAddr1
			and #%11110000
			bcc mPMzero	;from the cmp #8 above
			ora #%00001000 
mPMzero		sta monAddr1
			
			;MSB remains as is.			
			
			
printMem:	;Print Addresses and data from addr1 to addr2	
			jsr shCursor
pMLineLoop	lda #'\n'
			jsr vgaTerm
			lda #'\r'
			jsr vgaTerm
			
			;Print address
			lda monAddr1+1
			jsr mPrByte
			lda monAddr1
			jsr mPrByte
			
			lda #':'
			jsr vgaTerm
			
			;Print 8 bytes in that same line
			ldy #0
mPMPLoop	lda (monAddr1), y
			jsr mPrByte
			iny
			lda (monAddr1), y
			jsr mPrByte
			iny
			lda #' '
			jsr vgaTerm
			cpy #8
			bcc mPMPLoop
			
			dec textVIdx ;So no blank lines appear.
						 ;modified so it works better on serial terminal.
			
			;Increment address 
			lda monAddr1
			clc
			adc #$8
			sta monAddr1
			bcc mPMISkip ;No need to increment higher byte
			inc monAddr1+1
			beq mPMEnd; If we are rolling to 0, we are done.
			
mPMISkip		;Compare with monAddr2
			lda monAddr1+1
			cmp monAddr2+1
			beq mPMcmpLo ;Equal, we need to compare lower bits
			bcc pMLineLoop ;MSB of addr is < MSB addr2, we continue
			bcs mPMEnd ;MSB of addr is > MSB addr2, we are done
			
mPMcmpLo	lda monAddr1
			cmp monAddr2
			bcc pMLineLoop ;addr < addr2, we continue
			beq	pMLineLoop ;addr = addr2, we continue
			bcs mPMEnd ;addr > addr2, we are done
			
mPMEnd		rts


;################PRINT MEMORY AS ASCIII###########################
printAscii:	;Print Addresses and data from addr1 to addr2	
			jsr shCursor
pALineLoop	lda #'\n'
			jsr vgaTerm
			lda #'\r'
			jsr vgaTerm
			;Print address
			lda monAddr1+1
			jsr mPrByte
			lda monAddr1
			jsr mPrByte
			
			lda #':'
			jsr vgaTerm
			
			;Print 8 bytes in that same line
			ldy #0
mPAPLoop	lda (monAddr1), y
			jsr vgaChar
			inc textHIdx
			
			cmp #' '
			bcc pANoPr
			cmp #$7f
			bcs pANoPr
			bcc pAPrAble
pANoPr		lda #'.'
pAPrAble	jsr uartSend

			iny
			
			cpy #$10
			bcs mPAPLEnd ;End of line
			
			cpy #$08 ;Put a space between 8 char blocks
			bne mPAPLoop
			
			lda #' '
			jsr vgaTerm
			
			bne mPAPLoop ;Always taken
mPAPLEnd		
	
			;Increment address 
			lda monAddr1
			clc
			adc #$10
			sta monAddr1
			bcc mPAISkip ;No need to increment higher byte
			inc monAddr1+1
			beq mPAEnd; If we are rolling to 0, we are done.
			
mPAISkip	;Compare with monAddr2
			lda monAddr1+1
			cmp monAddr2+1
			beq mPAcmpLo ;Equal, we need to compare lower bits
			bcc pALineLoop ;MSB of addr is < MSB addr2, we continue
			bcs mPAEnd ;MSB of addr is > MSB addr2, we are done
			
mPAcmpLo	lda monAddr1
			cmp monAddr2
			bcc pALineLoop ;addr < addr2, we continue
			beq	pALineLoop ;addr = addr2, we continue
			bcs mPAEnd ;addr > addr2, we are done
			
mPAEnd		rts
			
;############PRINT ONE BYTE (A) AS TWO DIGIT ASCII HEX############
mPrByte:	pha

			lsr	a ;M.S. Nibble
			lsr	a
			lsr	a
			lsr	a
			jsr mPrHex	;Print hex
			
			pla ;L.S. Nibble
			and #%00001111
			
			;Falls through 
mPrHex:		clc
			adc #'0'
			cmp #'9'+1
			bcc mPHEnd ;Lands in '0'-'9', we are done
			clc
			adc #'A'-'9'-1
mPHEnd		jsr vgaTerm
			rts	;First time returns lo L.S. Nibble, second, mPrByte rts
			
			
			
			
			
			
;************************************************************
;*					 PRINT STRING 							*
;************************************************************
;Prints string (max 256 char) found at strAddr
printStr: 	pha ;push A 
			tya
			pha ;push Y
			
			ldy #0
			
psLoop		lda (strAddr) ,y
			beq psEnd ;Check null terminator.
			jsr vgaTerm
	
			iny
			bne psLoop ;Next char, if we are not wrapping around.
			
psEnd		pla ;Retrieve Y,A
			tay
			pla
			rts
;************************************************************
;*				 			CURSOR 							*
;************************************************************	
;Shows/hides cursor. Just inverts some pixels in a character space.		
shCursor: 	pha ;push A 
			tya
			pha ;push Y

			;HI VRAM INDEX
			lda textVIdx
			and #%00001111
			ora #hi vRAMbase
			sta vRAMIdx +1
			
			;LOW VRAM INDEX
			lda textHIdx
			sta vRAMIdx
			
			;Invert the 8 bytes that form a character in vRAM
			ldy #$0	;vRAM Index
			
shLoop:		lda (vRAMIdx),y
			eor #%10000000
			sta (vRAMIdx),y
	
			tya 
			clc
			adc #$20 ;Add $20 to vRAM index -> Next line
			tay
			bne shLoop ;Last addition: $E0 + $20 = $00

			pla ;Retrieve Y,X,A
			tay
			pla
			rts
	
;************************************************************
;*				 	VGA TEXT TERMINAL 						*
;************************************************************
;Makes the VGA video card act as a dumb terminal (Character in a).
		
vgaTerm:  	jsr uartSend ;Send char through uart as well.

			bit uVgaDis ;Skip vga if only using serial, for speed.
			bpl vtEnabled
			rts
			
vtEnabled:	pha ;push A
			txa
			tsx
			pha ;push X
			tya
			pha ;push Y
			inx
			lda $100,x ; get A from the stack
			
			
			;Special characters
			cmp #'\n'
			beq vtLF
			
			cmp #'\b'
			beq vtBS
			
			cmp #'\r'
			beq vtCR
		
			jmp vtChar
			
			;Line feed
vtLF	 	lda #$0			
			sta textHIdx
			inc textVIdx
			jmp vtScroll
			
			;Backspace
vtBS		dec textHIdx
			bpl vtBSclr	;If HIdx negative, decrease VIdx etc.
			ldx #24
			stx textHIdx
			dec textVIdx
			bpl vtBSclr
			inc textVIdx
			ldx #0
			stx textHIdx
			
vtBSclr		lda #' ' 
			jsr vgaChar
			jmp vtEnd
			
vtCR		lda #0
			sta textHIdx
			jmp vtEnd
			
			;Print regular characters
vtChar		jsr vgaChar
			inc textHIdx
			
			ldy textHIdx ;If HIdx more than 25, next line
			cpy #25
			bcc vtEnd
			ldy #$0
			sty textHIdx
			inc textVIdx
			
vtScroll	ldy textVIdx ;If VIdx more than 16, scroll
			cpy #16
			bcc vtEnd
			ldy #15
			sty textVIdx
			
			jsr vgaScroll
			
vtEnd:		pla ;Retrieve Y,X,A
			tay
			pla
			tax
			pla
			rts
			
			
;************************************************************
;*		 			VGA CLEAR SCREEN						*
;************************************************************	
;Clears the screen.
vgaCls:		pha
			tya
			pha
			
			;Setup 16 bit scren index 
			ldy #$0
			sty vRAMIdx
			tya
			
			ldy #(hi vRAMbase)
			sty vRAMIdx +1
			 
			
vcNextLine:	ldy #$0		
vcLineLoop: dey
			sta (vRAMIdx), y ; Clear line
			cpy #$0
			bne vcLineLoop
			
			ldy vRAMIdx +1 ;Increase line index
			iny
			sty vRAMIdx +1
			cpy #(hi vRAMbase) +$10 ;Check if we have reached the bottom
			bcc vcNextLine
			
			pla
			tay
			pla
			rts
			
			
;************************************************************
;*				 		VGA SCROLL	 						*
;************************************************************		
; Scrolls the screen one character up (8 lines)
vgaScroll:	pha ;push A
			txa
			pha ;push X
			tya
			pha ;push Y
			
			
			;Set up two line indexes, one to read from and one to copy from.
			ldy #(hi vRAMbase)
			sty vRAMIdx +1
			ldy #$0
			sty vRAMIdx 
			sty vRAMIdx2 
			
			ldy #(hi vRAMbase) +1
			sty vRAMIdx2 +1
			
			
			
vsNextLine:	ldy #$0		
vsLineLoop: dey
			lda (vRAMIdx2), y ; Copy lower line
			sta (vRAMIdx), y ; Save in upper line
			cpy #$0
			bne vsLineLoop
			
			inc vRAMIdx +1 ;Increase line index
			ldy vRAMIdx2 +1
			iny 
			sty vRAMIdx2 +1
			cpy #(hi vRAMbase) +$10 ;Check if we have reached the bottom
			bcc vsNextLine
			
			;Clear last line
			lda #$0
			ldy #$0
vsClrLoop:	sta (vRAMIdx), y
			dey
			bne vsClrLoop

vsEnd		pla ;Retrieve Y,X,A
			tay
			pla
			tax
			pla
			rts
			
;************************************************************
;*				 		VGA CHAR B	 						*
;************************************************************		
; Prints character in char at textHIdx, textVIdx
; For use by BASIC. Falls through vgaChar.
vgaCharB:	lda char
			
;************************************************************
;*				 		VGA CHAR	 						*
;************************************************************		
; Prints character in a at textHIdx, textVIdx

vgaChar:	pha ;push A
			txa
			tsx
			pha ;push X
			tya
			pha ;push Y
			inx
			lda $100,x ; get A from the stack
			
			sta tmp2 ; G21 Save ascii, later check for MSb set.
			
			ldx #0
			stx cROMIdx
			stx cROMIdx +1 
			
			;Get ascii bitmap address
			and #%01111111
			;Multiply ASCCI by 8
			asl a
			rol cROMIdx +1
			asl a
			rol cROMIdx +1
			asl a
			rol cROMIdx +1

			;Add value to Char ROM base address
			clc
			adc #lo charRom
			sta cROMIdx
			lda cROMIdx+1
			adc #hi charRom
			sta cROMIdx+1
			
			;Get screen address
			
			;HI VRAM INDEX
			lda textVIdx
			and #%00001111
			ora #hi vRAMbase
			sta vRAMIdx +1
			
			;LOW VRAM INDEX
			lda textHIdx
			sta vRAMIdx
			
			;Copy the 8 bytes that form a character from cROm to vRAM
			ldy #$0	;Memory index
			sty tmp1;cROM Index
			ldx #$0 ;vRAM Index
			
			lda tmp2  		;G21 Print char in reverse video
			bmi vcInvLoop	;If MSb set.
			
vcCharLoop:	ldy tmp1
			inc tmp1
			
			lda (cROMIdx),y ;Copy from cROM
			stx tmp2 ;There is no txy instruction available :(
			ldy tmp2
			sta (vRAMIdx),y ;Save in vRAM
		
			txa
			clc
			adc #$20 ;Add $20 to vRAM index -> Next line
			tax
			bne vcCharLoop ;Last addition: $E0 + $20 = $00
			beq vcEnd ;Always taken
			
vcInvLoop:	ldy tmp1 ;G21 Print char in reverse video.
			inc tmp1
			
			lda (cROMIdx),y ;Copy from cROM
			stx tmp2 ;There is no txy instruction available :(
			ldy tmp2
			eor #$ff ;We invert all pixels, reverse text.
			sta (vRAMIdx),y ;Save in vRAM
		
			txa
			clc
			adc #$20 ;Add $20 to vRAM index -> Next line
			tax
			bne vcInvLoop ;Last addition: $E0 + $20 = $00
			
vcEnd		pla ;Retrieve Y,X,A
			tay
			pla
			tax
			pla
			rts
			
			
;************************************************************
;*					KEYBOARD HANDLER						*
;************************************************************	

;kbFlags : Release (7), Shift(6), Control(5), Special(4), Caps(3)
;   			N          V

kbSpSet: 	lda #%00010000
			ora kbFlags
			sta kbFlags
			jmp kbEnd

kbRelSet: 	lda #%10000000
			ora kbFlags
			sta kbFlags
			jmp kbEnd

			;Shift key flags handling---------------------
kbShift:	bit kbFlags ;Test key release flag
			bmi kbShRel ;Key release set
			
			;Shift key was pressed:
			lda #%01000000 ;Set shift flag 
			ora kbFlags
			sta kbFlags
			jmp kbEnd
			
			;Shift key was released:
kbShRel:	lda #%10111111 ;Clear shift flag
			and kbFlags
			sta kbFlags
			
			
			;Caps key flag handling-------------------------
kbCaps:		bit kbFlags ;Test key release flag
			bmi kbClrRel ;Key release set, clear it and done.
			
			lda kbFlags ;Invert Caps flag
			eor #%00001000
			sta kbFlags
			jmp kbEnd
			
kbClrRel:	lda #%01111111 ;Clear key release flag
			and kbFlags
			sta kbFlags
			
			jmp kbEnd
			
			
			;Control key flags handling---------------------
kbCtrl:		bit kbFlags ;Test key release flag
			bmi kbCtRel ;Key release set
			
			;Control key was pressed:
			lda #%00100000 ;Set control flag 
			ora kbFlags
			sta kbFlags
			jmp kbEnd
			
			;Control key was released:
kbCtRel:	lda #%11011111 ;Clear control flag
			and kbFlags
			sta kbFlags
			
			lda #%01111111 ;Clear key release flag
			and kbFlags
			sta kbFlags
			
			jmp kbEnd
			
;############## KEYBOARD ENTRY POINT ################			
keyboard:	pha
			txa
			pha
		
			
			lda kbrd ;get key code
			;jsr mPrByte; DEBUG
			tax ;Save it in x as well, later as index in lookup table.
			
			;Special codes and flags------------------
			cmp #$e0 ;Special key prefix
			beq kbSpSet ;Set flag
			
			cmp #$f0 ;Key release prefix
			beq kbRelSet ;Set flag
			
			cmp #$12 ;Shift key (left)
			beq kbShift 
			cmp #$59 ;Shift key (right)
			beq kbShift 
			
			cmp #$58 ;Caps key
			beq kbCaps
			
			cmp #$14 ;Control key (left and (right preceeded 
			beq kbCtrl ;by E0, but gets ignored))
			
			bit kbFlags ;Test key release flag
			bpl kbNotRel ;Key release clear, continue.
			
			;Key release set, clear it and end.
			lda #%01111111 ;Clear key release flag
			and kbFlags
			sta kbFlags
			
			jmp kbEnd
			
			;Key combinations: Ctrl, Shift and Caps
kbNotRel:	lda #%00100000 ;Test control flag
			and kbFlags
			bne kbCtCodes ;Control flag set, handle Ctrl + X
			;Special flag handling HERE
			
			beq kbAscii; Always taken
			
			;Ctrl + X handling------------------------------
kbCtCodes:	lda kbToAscii, x; Translate to ascii
			cmp #'m'   ;Added in G17. Ctrl+M returns to the monitor
			beq kbCtM
			cmp #'c'
			beq kbCtC
			jmp kbEnd

kbCtC		lda #$03 ;Ctrl c ASCII code. Stops basic execution.
			jmp kbChar
kbCtM		cli ;Re-enable interrupts
			jmp monitor
			jmp kbEnd ; TO BE COMPLETED
			
			;Here we are dealing with ASCII, no longer Scan Codes.
kbAscii:	lda kbToAscii, x ;Now that we are done with the keys, traslate to ASCII. Index still in X
				
			;Shift action----------------------------------
			bit kbFlags ;Test shift flag
			bvc kbNoShift ;Shift flag not set, check caps flag.
			
			;Shift flag set, modify character.
			;Special cases: This is for a Spanish keyboard layout.
			cmp #'+'
			beq kbShPlus
			cmp #'-'
			beq kbShMinus
			cmp #"'"
			beq kbShApos
			cmp #'<'
			beq kbShSmall
			cmp #'.'
			beq kbShDot
			cmp #','
			beq kbShComma
			cmp #'7'
			beq kbSh7
			cmp #'0'	;Avoid shifting BS, CR, SPACE
			beq kbSh0;Equals sign.
			bcc kbShNumber
			sec
			sbc #16
			cmp #$30	
			bcc kbShNumber ;If less than a $30, it was a number.
			sbc #16	;No need to set carry
kbShNumber:	jmp kbChar


kbShPlus:	lda #'*'
			bne kbChar ;Always taken
kbShMinus:	lda #'_'
			bne kbChar ;Always taken
kbShApos:	lda #'?'
			bne kbChar ;Always taken
kbShSmall:	lda #'>'
			bne kbChar ;Always taken
kbShDot:	lda #':'
			bne kbChar ;Always taken
kbShComma:	lda #';'
			bne kbChar ;Always taken
kbSh7:		lda #'/'
			bne kbChar ;Always taken
kbSh0:		lda #'='
			bne kbChar ;Always taken
			
			;CAPS handling-----------------------------------
kbNoShift:	tax ;Save char in x
			lda #%00001000 ;Test caps flag
			and kbFlags
			bne kbCapsMod ;Caps flag set, mod. character
			txa
			jmp kbChar
kbCapsMod:	txa ;Retrieve char from x. If  'a'...'z', turn to upper case.
			cmp #'a'
			bcc kbChar ;Less than 'a', not a letter
			cmp #'z'+1
			bcs kbChar ;More than 'z', not a letter
			sbc #32-1 ;-1 because carry (borrow) is cleared.
			jmp kbChar
			
kbChar:		sta kbBuffer	;Put character in buffer
kbEnd:		pla
			tax
			pla
			
			rts
			


			

			
;************************************************************
;*				KEYBOARD LOOK-UP TABLE						*
;************************************************************		
;				 0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
kbToAscii:	db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$00
			db	' ', ' ', ' ', ' ', ' ', 'q', '1', ' ', ' ', ' ', 'z', 's', 'a', 'w', '2', ' ' ;$10
			db	' ', 'c', 'x', 'd', 'e', '4', '3', ' ', ' ', ' ', 'v', 'f', 't', 'r', '5', ' ' ;$20
			db	' ', 'n', 'b', 'h', 'g', 'y', '6', ' ', ' ', ' ', 'm', 'j', 'u', '7', '8', ' ' ;$30
			db	' ', ',', 'k', 'i', 'o', '0', '9', ' ', ' ', '.', '-', 'l', ' ', 'p', "'", ' ' ;$40
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ','\r', '+', ' ', ' ', ' ', ' ' ;$50
			db	' ', '<', ' ', ' ', ' ', ' ','\b', ' ', ' ', '1', ' ', '4', '7', ' ', ' ', ' ' ;$60
			db	'0', $5f, '2', '5', '6', '8', ' ', ' ', ' ', '+', '3', ' ', '*', '9', ' ', ' ' ;$70
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$80
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$90
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$a0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$b0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$c0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$d0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$e0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$f0
			
;************************************************************
;*				 		CHARACTER ROM  						*
;************************************************************	
;http://pelulamu.net/unscii/ , unscii-8-thin

charRom:	db  $C0, $A0, $AA, $AA, $AA, $0A, $0E, $00 ; $0
			db	$E0, $80, $EA, $2A, $EE, $0A, $0A, $00 ; $1
			db	$E0, $80, $EA, $2A, $E4, $0A, $0A, $00 ; $2
			db	$E0, $80, $CA, $8A, $E4, $0A, $0A, $00 ; $3
			db	$E0, $80, $CE, $84, $E4, $04, $04, $00 ; $4
			db	$E0, $80, $CE, $8A, $EA, $0E, $04, $00 ; $5
			db	$E0, $A0, $EA, $AA, $AC, $0A, $0A, $00 ; $6
			db	$C0, $A0, $C8, $A8, $C8, $08, $0E, $00 ; $7
			db	$C0, $A0, $CE, $A8, $CE, $02, $0E, $00 ; $8
			db	$A0, $A0, $EE, $A4, $A4, $04, $04, $00 ; $9
			db	$80, $80, $8E, $88, $EC, $08, $08, $00 ; $a
			db	$A0, $A0, $AE, $A4, $44, $04, $04, $00 ; $b
			db	$E0, $80, $CE, $88, $8C, $08, $08, $00 ; $c
			db	$E0, $80, $8E, $8A, $EE, $0C, $0A, $00 ; $d
			db	$E0, $80, $EE, $2A, $EA, $0A, $0E, $00 ; $e
			db	$E0, $80, $EE, $24, $E4, $04, $0E, $00 ; $f
			db	$C0, $A0, $A8, $A8, $C8, $08, $0E, $00 ; $10
			db	$C0, $A0, $A4, $AC, $C4, $04, $0E, $00 ; $11
			db	$C0, $A0, $AE, $A2, $CE, $08, $0E, $00 ; $12
			db	$C0, $A0, $AE, $A2, $C6, $02, $0E, $00 ; $13
			db	$C0, $A0, $AA, $AA, $CE, $02, $02, $00 ; $14
			db	$E0, $A0, $AA, $AA, $AC, $0A, $0A, $00 ; $15
			db	$E0, $80, $EA, $2A, $EE, $04, $04, $00 ; $16
			db	$E0, $80, $CC, $8A, $EC, $0A, $0C, $00 ; $17
			db	$E0, $80, $8E, $8A, $EA, $0A, $0A, $00 ; $18
			db	$E0, $80, $CA, $8E, $EA, $0A, $0A, $00 ; $19
			db	$3C, $66, $66, $30, $18, $00, $18, $00 ; $1a
			db	$E0, $80, $CE, $88, $E8, $08, $0E, $00 ; $1b
			db	$E0, $80, $CE, $88, $8E, $02, $0E, $00 ; $1c
			db	$E0, $80, $AE, $A8, $EE, $02, $0E, $00 ; $1d
			db	$E0, $A0, $EE, $C8, $AE, $02, $0E, $00 ; $1e
			db	$A0, $A0, $AE, $A8, $EE, $02, $0E, $00 ; $1f                          
			db	$00, $00, $00, $00, $00, $00, $00, $00 ; $20
			db	$10, $10, $10, $10, $00, $00, $10, $00 ; $21
			db	$24, $24, $24, $00, $00, $00, $00, $00 ; $22
			db	$24, $24, $7E, $24, $7E, $24, $24, $00 ; $23
			db	$10, $3C, $50, $38, $14, $78, $10, $00 ; $24
			db	$00, $62, $64, $08, $10, $26, $46, $00 ; $25
			db	$30, $48, $48, $30, $4A, $44, $3A, $00 ; $26
			db	$08, $10, $20, $00, $00, $00, $00, $00 ; $27
			db	$08, $10, $20, $20, $20, $10, $08, $00 ; $28
			db	$20, $10, $08, $08, $08, $10, $20, $00 ; $29
			db	$10, $54, $38, $10, $38, $54, $10, $00 ; $2a
			db	$00, $10, $10, $7C, $10, $10, $00, $00 ; $2b
			db	$00, $00, $00, $00, $00, $08, $08, $10 ; $2c
			db	$00, $00, $00, $00, $7C, $00, $00, $00 ; $2d
			db	$00, $00, $00, $00, $00, $00, $10, $00 ; $2e
			db	$02, $04, $08, $10, $20, $40, $80, $00 ; $2f
			db	$38, $44, $4C, $54, $64, $44, $38, $00 ; $30
			db	$10, $30, $50, $10, $10, $10, $7C, $00 ; $31
			db	$38, $44, $04, $08, $30, $40, $7C, $00 ; $32
			db	$38, $44, $04, $18, $04, $44, $38, $00 ; $33
			db	$08, $18, $28, $48, $7C, $08, $08, $00 ; $34
			db	$7C, $40, $78, $04, $04, $44, $38, $00 ; $35
			db	$1C, $20, $40, $78, $44, $44, $38, $00 ; $36
			db	$7C, $04, $08, $10, $20, $20, $20, $00 ; $37
			db	$38, $44, $44, $38, $44, $44, $38, $00 ; $38
			db	$38, $44, $44, $3C, $04, $08, $70, $00 ; $39
			db	$00, $00, $10, $00, $00, $10, $00, $00 ; $3a
			db	$00, $00, $10, $00, $00, $10, $10, $20 ; $3b
			db	$08, $10, $20, $40, $20, $10, $08, $00 ; $3c
			db	$00, $00, $7C, $00, $7C, $00, $00, $00 ; $3d
			db	$20, $10, $08, $04, $08, $10, $20, $00 ; $3e
			db	$3C, $42, $04, $08, $08, $00, $08, $00 ; $3f
			db	$3C, $42, $4A, $56, $4C, $40, $3C, $00 ; $40
			db	$18, $24, $42, $7E, $42, $42, $42, $00 ; $41
			db	$7C, $42, $42, $7C, $42, $42, $7C, $00 ; $42
			db	$3C, $42, $40, $40, $40, $42, $3C, $00 ; $43
			db	$78, $44, $42, $42, $42, $44, $78, $00 ; $44
			db	$7E, $40, $40, $78, $40, $40, $7E, $00 ; $45
			db	$7E, $40, $40, $78, $40, $40, $40, $00 ; $46
			db	$3C, $42, $40, $4E, $42, $42, $3C, $00 ; $47
			db	$42, $42, $42, $7E, $42, $42, $42, $00 ; $48
			db	$38, $10, $10, $10, $10, $10, $38, $00 ; $49
			db	$04, $04, $04, $04, $04, $44, $38, $00 ; $4a
			db	$42, $44, $48, $70, $48, $44, $42, $00 ; $4b
			db	$40, $40, $40, $40, $40, $40, $7E, $00 ; $4c
			db	$42, $66, $5A, $42, $42, $42, $42, $00 ; $4d
			db	$42, $62, $52, $4A, $46, $42, $42, $00 ; $4e
			db	$3C, $42, $42, $42, $42, $42, $3C, $00 ; $4f
			db	$7C, $42, $42, $7C, $40, $40, $40, $00 ; $50
			db	$3C, $42, $42, $42, $4A, $44, $3A, $00 ; $51
			db	$7C, $42, $42, $7C, $48, $44, $42, $00 ; $52
			db	$3C, $42, $40, $3C, $02, $42, $3C, $00 ; $53
			db	$7C, $10, $10, $10, $10, $10, $10, $00 ; $54
			db	$42, $42, $42, $42, $42, $42, $3C, $00 ; $55
			db	$42, $42, $42, $42, $42, $24, $18, $00 ; $56
			db	$42, $42, $42, $42, $5A, $66, $42, $00 ; $57
			db	$42, $42, $24, $18, $24, $42, $42, $00 ; $58
			db	$44, $44, $44, $38, $10, $10, $10, $00 ; $59
			db	$7C, $04, $08, $10, $20, $40, $7C, $00 ; $5a
			db	$38, $20, $20, $20, $20, $20, $38, $00 ; $5b
			db	$80, $40, $20, $10, $08, $04, $02, $00 ; $5c
			db	$38, $08, $08, $08, $08, $08, $38, $00 ; $5d
			db	$10, $28, $44, $00, $00, $00, $00, $00 ; $5e
			db	$00, $00, $00, $00, $00, $00, $00, $7C ; $5f
			db	$20, $10, $08, $00, $00, $00, $00, $00 ; $60
			db	$00, $00, $38, $04, $3C, $44, $3C, $00 ; $61
			db	$40, $40, $78, $44, $44, $44, $78, $00 ; $62
			db	$00, $00, $38, $44, $40, $44, $38, $00 ; $63
			db	$04, $04, $3C, $44, $44, $44, $3C, $00 ; $64
			db	$00, $00, $38, $44, $7C, $40, $38, $00 ; $65
			db	$0C, $12, $10, $7C, $10, $10, $10, $00 ; $66
			db	$00, $00, $3C, $44, $44, $3C, $04, $38 ; $67
			db	$40, $40, $78, $44, $44, $44, $44, $00 ; $68
			db	$10, $00, $30, $10, $10, $10, $38, $00 ; $69
			db	$04, $00, $0C, $04, $04, $04, $24, $18 ; $6a
			db	$40, $40, $44, $48, $70, $48, $44, $00 ; $6b
			db	$30, $10, $10, $10, $10, $10, $38, $00 ; $6c
			db	$00, $00, $68, $54, $54, $54, $54, $00 ; $6d
			db	$00, $00, $78, $44, $44, $44, $44, $00 ; $6e
			db	$00, $00, $38, $44, $44, $44, $38, $00 ; $6f
			db	$00, $00, $78, $44, $44, $78, $40, $40 ; $70
			db	$00, $00, $3C, $44, $44, $3C, $04, $04 ; $71
			db	$00, $00, $5C, $60, $40, $40, $40, $00 ; $72
			db	$00, $00, $3C, $40, $38, $04, $78, $00 ; $73
			db	$20, $20, $78, $20, $20, $24, $18, $00 ; $74
			db	$00, $00, $44, $44, $44, $44, $38, $00 ; $75
			db	$00, $00, $44, $44, $44, $28, $10, $00 ; $76
			db	$00, $00, $44, $54, $54, $54, $28, $00 ; $77
			db	$00, $00, $44, $28, $10, $28, $44, $00 ; $78
			db	$00, $00, $44, $44, $44, $3C, $04, $38 ; $79
			db	$00, $00, $7C, $08, $10, $20, $7C, $00 ; $7a
			db	$18, $20, $20, $40, $20, $20, $18, $00 ; $7b
			db	$10, $10, $10, $10, $10, $10, $10, $10 ; $7c
			db	$30, $08, $08, $04, $08, $08, $30, $00 ; $7d
			db	$20, $54, $08, $00, $00, $00, $00, $00 ; $7e
			db	$C0, $A0, $AE, $A4, $C4, $04, $04, $00 ; $7f



;************************************************************
;*					IRQ										*
;************************************************************

IRQISR:		pha
			txa
			pha
			
			;DETECT BRK
			tsx		   ; Get SR from stack
			lda $103,x ; 100 + 2(2 x pha) + 1
			
			and #%00010000 ;If b set, handle brk
			bne iBrkSub
			
			;IRQ
			;UART
			lda uartStatus
			and uInt
			beq iNoSerial
			
			jsr uartHadler
			
			bvc iiEnd
			
			;KEYBOARD
iNoSerial
			jsr keyboard	;Else, it was the keyboard.

iiEnd		pla
			tax
			pla
			rti
			
iBrkSub:	pla
			tax
			pla
			cli	;Upon brk, re-enable interrupts and jump to the monitor
			jmp brkMonitor		
			

NMIISR:
;************************************************************
;*			 SYSTEM FUNCTIONS JUMP TABLE 					*
;************************************************************
			org $ffc0
			jmp monitor		; Jumps to the ROM monitor
			jmp vgaCharB	; Prints a character in char at textHIdx, textVIdx
			jmp vgaCls		; Clears the screen
			jmp vgaScroll	; Scrolls the screen 8 pixels up
			jmp gPlotPix	; Plots point at (gX0, gY0)
			jmp gLine		; Draws line between (gX0, gY0) and (gX1, gY1) CHANGES COORDINATES (DESTRUCTIVE)!!!
			
;************************************************************
;*					RESET & IRQ VECTORS						*
;************************************************************			
			
			; RESET vector
			org $fffa
			dw	NMIISR
			dw  RESET
			dw  IRQISR
.end


