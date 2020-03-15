; LCD & KEYBOARD ROUTINES V20

; By JuanGg on March 2020
; To assemble with as65: http://web.archive.org/web/20190301123637/http://www.kingswood-consulting.co.uk/assemblers/
; Still needs documentation. Keep in mind that I have no idea of what I'm doing.

;Seems to work allright. Maybe hardware issues.
;Preliminary keyboard working. Higher case letters & numbers working
;Scrolling and cursor fixed.
;Resets when it feels like. Make false NMIs?

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

.6502   

lcd10 		equ	$9C02  
lcd11		equ	$9C03 
lcd20 		equ	$9C04  
lcd21		equ	$9C05 

kbrd		equ $9C08

lcdIndex 	equ $0
kbBuffer    equ $1
kbFlags		equ $2


			org $c000
			
			;RESET
			
			ldx #$ff
			txs
			
			lda #$0
			sta kbFlags
			
			jsr lcdInit
			
			
			;-----------------------------------------
			;Print hello world:
			lda #0
			sta lcdIndex

			lda #'1'
			jsr lcdTerm
			
			lda #'H'
			jsr lcdTerm
			
			lda #'O'
			jsr lcdTerm	
			
			lda #'L'
			jsr lcdTerm
			
			lda #'A'
			jsr lcdTerm	
			
			lda #'\n'
			jsr lcdTerm	
			
			lda #'2'
			jsr lcdTerm
			
			lda #'H'
			jsr lcdTerm
			
			lda #'O'
			jsr lcdTerm	
			
			lda #'L'
			jsr lcdTerm
			
			lda #'A'
			jsr lcdTerm	
			
			lda #'\n'
			jsr lcdTerm	
			
			
			lda #'3'
			jsr lcdTerm
			
			lda #'H'
			jsr lcdTerm
			
			lda #'O'
			jsr lcdTerm	
			
			lda #'L'
			jsr lcdTerm
			
			lda #'A'
			jsr lcdTerm	
			
			lda #'\n'
			jsr lcdTerm	
			
			
			lda #'4'
			jsr lcdTerm
			
			lda #'H'
			jsr lcdTerm
			
			lda #'O'
			jsr lcdTerm	
			
			lda #'L'
			jsr lcdTerm
			
			lda #'A'
			jsr lcdTerm	
			
			lda #'\n'
			jsr lcdTerm	
			
			lda #'5'
			jsr lcdTerm
			
			lda #'H'
			jsr lcdTerm
			
			lda #'O'
			jsr lcdTerm	
			
			lda #'L'
			jsr lcdTerm
			
			lda #'A'
			jsr lcdTerm	
			
			lda #'\n'
			jsr lcdTerm	 
			
			
			
loop 		ldx lcdIndex
			clc
			bcc loop
			
NMIISR:	
			jmp loop
			
			
;************************************************************
;*					KEYBOARD HANDLER						*
;************************************************************	

;kbFlags : Release (7), Shift(6), Control(5), Special(4)
;   			N          V
keyboard:	pha
			txa
			pha
		

			lda kbrd ;get key code
			tax ;Save it in x as well
			
			cmp #$e0 ;Special key prefix
			beq kbSpSet ;Set flag
			
			cmp #$f0 ;Key release prefix
			beq kbRelSet ;Set flag
			
			;Special flag handling HERE
			
			cmp #$12 ;Shift key (left)
			beq kbShift 
			
			cmp #$14 ;Control key (left)
			beq kbCtrl
			
			
			bit kbFlags ;Test key release flag
			bpl kbNotRel ;Key release clear, continue.
			
			;Key release set, clear it and end.
			lda #%01111111 ;Clear key release flag
			and kbFlags
			sta kbFlags
			
			jmp kbEnd
			
kbNotRel:			
			lda #%00100000 ;Test control flag
			and kbFlags
			bne kbCtCodes ;Control flag set, handle Ctrl + X
			 
			lda kbToAscii, x ;Now that we are done with the keys, traslate to ASCII. Index still in X
				
			;Shift action----------------------------------
			bit kbFlags ;Test shift flag
			bvc kbChar ;Shift flag not set, continue (output char)
			
			;Shift flag set, modify character.
			sec
			sbc #16
			cmp #$30	
			bcc kbShNumber ;If less than a $30, it was a number.
			sbc #16	;No need to set carry
kbShNumber:	jmp kbChar
	
			

kbRelSet: 	lda #%10000000
			ora kbFlags
			sta kbFlags
			jmp kbEnd

kbSpSet: 	lda #%00010000
			ora kbFlags
			sta kbFlags
			jmp kbEnd
			

			
			;Shift key handling---------------------
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
			
			lda #%01111111 ;Clear key release flag
			and kbFlags
			sta kbFlags
			
			jmp kbEnd
			
			
			;Control key handling---------------------
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
			
kbCtCodes:	jmp kbEnd ; TO BE COMPLETED

	
			
			
			
kbChar:		sta kbBuffer	;Put character in buffer
			jsr lcdTerm		;TEMP. Print char.
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
			db	' ', ' ', 'k', 'i', 'o', '0', '9', ' ', ' ', ' ', ' ', 'l', ' ', 'p', ' ', ' ' ;$40
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ','\n', ' ', ' ', ' ', ' ', ' ' ;$50
			db	' ', ' ', ' ', ' ', ' ', ' ','\b', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$60
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$70
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$80
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$90
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$a0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$b0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$c0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$d0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$e0
			db	' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' ;$f0
			
			



;************************************************************
;*					IRQ										*
;************************************************************

IRQISR:		jsr keyboard


			rti		



;************************************************************
;*					LCD TERMINAL							*
;************************************************************
;Makes the lcd act as a dumb terminal (Character in a).
		
lcdTerm:  	pha ;push A
			txa
			tsx
			pha ;push X
			tya
			pha ;push Y
			inx
			lda $100,x ; get A from the stack

			
			;Special characters
			cmp #'\n'
			beq ltLF
			cmp #'\b'
			beq ltBS
			
			;Print regular characters
			
ltCh:		tax ;put character on x register
			jsr lcdChar
			
			ldy lcdIndex
			iny
			sty lcdIndex
			cpy #160
			bcc ltChEnd	;If less than 160 skip scrolling.
			jsr lineFeed ; Do lineFeed (and scroll) when end of the display is reached.
			
ltChEnd:	jmp ltEnd

ltLF:		jsr lineFeed
			jmp ltEnd

			;Back Space
ltBS:		ldx lcdIndex ;Stop at index = 0
			beq ltEnd
			dec lcdIndex
			ldx #' '
			jsr lcdChar
			
			
ltEnd:		ldx #$0		;This sets cursor at current index
			jsr lcdChar 

			pla ;Retrieve Y,X,A
			tay
			pla
			tax
			pla
			rts
			
			
;************************************************************
;*					LCD CHAR								*
;************************************************************
;Prints a character (in x) at lcdIndex. If 0, just sets cursor position.	
lcdChar:	pha
			
			lda lcdIndex
			
			;Identify current line
lcIdLine:	cmp #40
			bcc lcLcd1_1
			cmp #80
			bcc lcLcd1_2
			cmp #120
			bcc lcLcd2_1
			
lcLcd2_2:	clc
			adc #24 ;Second line starts at $40
lcLcd2_1:	sec
			sbc #80 ;Index to 0...79
			
			;Set address
			ora #$80 
			sta lcd20
			jsr lcdbusy
			
			;Print char
			cpx #$0 ;If char code equals 0, just set cursor address
			beq lcLcd1Cur
			stx lcd21
			jsr lcdbusy
			
			;Show cursor only on "active" LCD
			;          DCB
lcLcd1Cur:	lda #%00001110  ;Display on, cursor appear
			sta lcd20 
			lda #%00001100  ;Display on, hide cursor
			sta lcd10
			jsr lcdbusy
			
			jmp lcEnd
				
lcLcd1_2:	clc
			adc #24 ;Second line starts at $40			
lcLcd1_1:	;Set address
			ora #$80 
			sta lcd10
			jsr lcdbusy
			
			;Print char
			cpx #$0 ;If char code equals 0, just set cursor address
			beq lcLcd2Cur
			stx lcd11
			jsr lcdbusy
			
			;Show cursor only on "active" LCD
			;          DCB
lcLcd2Cur:	lda #%00001110  ;Display on, cursor appear
			sta lcd10 
			lda #%00001100  ;Display on, hide cursor
			sta lcd20
			jsr lcdbusy
			
		
lcEnd:		pla ;Retrieve A
			rts

;************************************************************
;*					LINE FEED								*
;************************************************************

			;Line feed----------------------
lineFeed:	pha ;Save A
			tya
			pha ;Save Y

			ldy lcdIndex
			lda #$0
			clc
			
			;Identify current line
			cpy #40
			bcc ltLine1
			cpy #80
			bcc ltLine2
			cpy #120
			bcc ltLine3
			
			;Go to next line or scroll as needed
ltLine4:	jsr lcdScroll 
			clc ;Lands here after a bpl, cpy sets carry.
ltLine3:	adc #40
ltLine2:	adc #40
ltLine1:	adc #40
			sta lcdIndex
			
			
			pla ;Retrieve Y
			tay
			pla ;Retrieve A 
			rts	
			
			
			




			
;************************************************************
;*					LCD BUSY								*
;************************************************************
			
;Waits for both LCD controllers to be available		
lcdbusy:	bit lcd10
			bmi lcdbusy
			bit lcd20
			bmi lcdbusy
			rts
		
;************************************************************
;*					LCD SCROLL								*
;************************************************************
;Scrolls the entire screen one place up			
lcdScroll:  pha ;Save A,X,Y
			txa
			pha
			tya
			pha

			ldy #$0 ;Column index starts at 0
			clc		;Clear carry for subsequent additions
			
			;LINE 2 to 1-------------------------------
			;Index of line 2
lcdScroll0:	tya
			adc #$40
			
			;Set address
			ora #$80 
			sta lcd10
			jsr lcdbusy
			
			;read char
			ldx lcd11
			jsr lcdbusy
			
			;Index of line 1
			tya
			
			;Set address
			ora #$80 
			sta lcd10
			jsr lcdbusy
			
			;store char
			stx lcd11
			jsr lcdbusy
			
			;LINE 3 to 2-----------------------------
			;Note lines 1 and 2 belong to lcd1 and 3 and 4 to lcd2
			
			;Index of line 3 is the same as line 1 ($0...$27). It is already ORed
			
			;Set address 
			;(ora #$80)
			sta lcd20
			jsr lcdbusy
			
			;read char
			ldx lcd21
			jsr lcdbusy
			
			;Index of line 2 ($40...$67)
			tya
			adc #$40
			
			;Set address 
			ora #$80 
			sta lcd10
			jsr lcdbusy
			
			;store char
			stx lcd11
			jsr lcdbusy
			
			;LINE 4 to 3 and clear 4-----------------------------
			;Index of line 4
			tya
			adc #$40
			
			;Set address
			ora #$80 
			sta lcd20
			jsr lcdbusy
			
			;read char
			ldx lcd21
			jsr lcdbusy
			
			;Set same address again
			;(ora #$80) 
			sta lcd20
			jsr lcdbusy
			
			;clear char (so line 4 is empty)
			lda #$20
			sta lcd21
			jsr lcdbusy
			
			;Index of line 3
			tya
			
			;Set address
			ora #$80 
			sta lcd20
			jsr lcdbusy
			
			;store char
			stx lcd21
			jsr lcdbusy
			
			
			;Next column
			iny
			cpy #$28
			bne lcdScroll0
			
			pla ;Retrieve Y,X,A
			tay
			pla
			tax
			pla
			
			rts




;************************************************************
;*					Delay loop								*
;************************************************************

delay:		pha
			txa
			pha 
			
			ldx #$ff ;* +- 15 clocks
			
delay0		dex 
			nop
			nop
			bne delay0
			
			pla
			tax
			pla
			rts
			
			
;************************************************************
;*					Initialize the LCD						*
;************************************************************
lcdInit:	pha
			txa
			pha
			
			sei
			
			lda #$3f   ;Special case function set
			sta lcd10 
			sta lcd20
			jsr delay
			
			lda #$3f
			sta lcd10 
			sta lcd20
			jsr delay
			
			lda #$3f
			sta lcd10 
			sta lcd20
			jsr delay
			
			lda #$3f  ;Function set
			sta lcd10 
			sta lcd20
			jsr lcdbusy
			
			lda #$0e  ;Display on, cursor appear
			sta lcd10 
			sta lcd20
			jsr lcdbusy
			
			lda #$01  ;Clear display
			sta lcd10 
			sta lcd20
			jsr lcdbusy
			
			lda #$06  ;Move cursor right
			sta lcd10 
			sta lcd20
			jsr lcdbusy
			
			pla
			tax
			pla
			
			cli
			rts

			
;************************************************************
;*					RESET & IRQ VECTORS						*
;************************************************************			
			
     ; RESET vector
     org $fffa
	 dw	 NMIISR
	 dw  $c000
	 dw  IRQISR
.end


