; Simple asteroids game for my Microcoded 6502 computer

; Works with the 25 x 16 character grid, so movements are quite clunky.
; Functionality is pretty limited, but it can be played.
; Delay loops are needed to slow it down, it can run quite faster than it does now.

; By JuanGg on July 2020
; https://juangg-projects.blogspot.com/
; http://forum.6502.org/viewtopic.php?f=12&t=5811

; To assemble with as65: http://web.archive.org/web/20190301123637/http://www.kingswood-consulting.co.uk/assemblers/
; Still needs documentation. Keep in mind that I have no idea of what I'm doing.




;************************************************************
;*			SYSTEM & OTHER CONSTANTS 						*
;************************************************************
monitor		equ $ffc0 ; Jumps to the ROM monitor
vgaCharB	equ $ffc3 ; Prints a character in char at textHIdx, textVIdx
vgaCls		equ $ffc6 ; Clears the screen
vgaScroll	equ $ffc9 ; Scrolls the screen 8 pixels up
gPlotPix	equ $ffcc ; Plots point at (gX0, gY0)
gLine		equ $ffcf ; Draws line between (gX0, gY0) and (gX1, gY1) CHANGES COORDINATES (DESTRUCTIVE)!!!
basicIn		equ $ffd2 ; If character from keyboard or uart, puts it in a and sets c. Else clears a and c.
mPrByte		equ $ffd5 ; Prints byte in A as two digit ascii hex.
vgaChar		equ $ffd8 ; Prints character in A at textHIdx, textVIdx
vgaTerm		equ $ffdb ; Screen acts as a dub terminal. Also sends via uart
printStr	equ $ffde ;Prints string (max 256 char) found at strAddr
;Keyboard
kbrd		equ $9C08

;VGA
vRAMbase	equ $7000
vgaColor    equ $9C10

;UART
uartData	equ $9000
uartStatus	equ $9001
;************************************************************
;*					 HOUSEKEEPING 							*
;************************************************************

			bss ;Uninitialized data segment
			org $14 ;...$5A free zero page locations not used by EhBASIC
					;For use by the monitor. 5A on can be used by the game as
					;it won't run simultaneously with BASIC.
			data	;Initialized data (e.g strings in ROM)
			org $bf00
			code ;Code segment. This is where the monitor and I/O routines live.
			org $a000

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
;################MONITOR####################
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
				  ; Bit 6: 0 cursor on, 1 cursor off.
				  
;MINI (Dis)assembler
asMNEM		ds	1
asAMOD		ds	1
aPrCnt		ds	2
mBIdx		ds	1

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

;##############GAME#################
			org $5a
gcAddr		ds	2 ;printGraph prints graphical char at this address.
nextAst		ds	1 ;offset from astList to reach last stored asteroid.
shipX		ds	1
shipY		ds	1
shipRot		ds	1
shipVX		ds	1
shipVY		ds	1
aType		ds	1
aPosX		ds	1
aPosY		ds	1
aVXY		ds	1

			; pSpeed
			; Bits 7,6: 00:Nothing 01:X+ 10:X- ;1
			; Bits 5,4: 00:Nothing 01:Y+ 10:Y-
			
			; Bits 3,2: 00:Nothing 01:X+ 10:X- ;2
			; Bits 1,0: 00:Nothing 01:Y+ 10:Y-
					
pSpeed		ds	1 	
p1X			ds	1
p1Y			ds	1
p2X			ds	1
p2Y			ds	1
lives		ds	1
rnSeed		ds	1
gameSpeed	ds	1
naCount		ds	1
points 		ds	2

;#############ASTEROID LIST############
;aType, aPosX, aPosY, aVXY
			org $300
astList:	ds 256


			code
;************************************************
;*				WELCOME SCREEN					*
;************************************************
			jsr vgaCls
			
			lda #0
			sta textHIdx
			lda #5
			sta textVIdx
			
			printMacro "       ASTEROIDS\n\n\r   Press ENTER to play\n\n\r Arrow keys:L.R&F:thrust\n\r     SPACE to fire"
			
wsSpaceLoop:jsr basicIn
			cmp #'\r'
			bne wsSpaceLoop
;************************************************
;*			INITIALIZE VARIABLES				*
;************************************************
			
gameInit:	jsr vgaCls ;Clear screen
			
			lda #0 ;Clear points
			sta points
			sta points +1
			
			lda #9 ;Set lives to 9
			sta lives
			
			lda #3 ;Number of asteroids at which new ones appear
			sta naCount
			
			lda #$0A ;Speed of the game
			sta gameSpeed
			
			lda #0	;Ship rotation and speed
			sta shipRot
			sta shipVX
			sta shipVY
			 
			lda #12  ;Ship position
			sta shipX
			lda #8
			sta shipY
			
			lda #0	;Clear asteroid list
			sta nextAst
						
;************************************************
;*				GAME LOOP						*
;************************************************
gameLoop:	;General game logic
			;-----------------------------
			;Check we have enough lives
			lda lives
			bpl glLivSkip
			lda #0 ;So that FF is not seen on end screen
			sta lives
			jmp endScreen
glLivSkip:
			
			;Add new asteroids when too few in play.
			lda nextAst
			cmp naCount
			bcs glNaSkip
			jsr newAst
			
			lda naCount ;But never with more than eight.
			cmp #8
			bcs glNaSkip
			inc naCount ;Increase difficulty.
glNaSkip:

			;Parameter displays
			;--------------------
			lda #0
			sta textHIdx
			sta textVIdx
			
			printMacro "Lives:"
			lda lives 
			jsr mPrByte
			
			printMacro " Points:"
			lda points +1
			jsr mPrByte
			lda points
			jsr mPrByte

			;Ship
			jsr controlShip
			jsr hideShip
			jsr moveShip
			jsr showShip
			
			;Projectiles
			jsr hideProj
			jsr moveProj
			jsr impHandler
			jsr showProj

			;Delay
			jsr delayLoop
			
			;Asteroids
			ldx #$FF
astLoop:	inx
			cpx nextAst
			beq astEnd
			jsr hideAst
			jsr moveAst
			jsr showAst
			jmp astLoop
astEnd:
			
			;Ship
			jsr controlShip
			jsr hideShip
			jsr moveShip
			jsr showShip
			
			;Projectiles
			jsr hideProj
			jsr moveProj
			jsr impHandler
			jsr showProj
			
			;Delay
			jsr delayLoop
			
			
			jmp gameLoop
			
;************************************************
;*				END SCREEN						*
;************************************************
endScreen:	
			lda #0
			sta textHIdx
			lda #7
			sta textVIdx
			printMacro "  GAME OVER! Points:"
			lda points +1
			jsr mPrByte
			lda points
			jsr mPrByte
			
			printMacro "\n\r   ENTER to play again"
			
esSpaceLoop:jsr basicIn
			cmp #'\r'
			bne esSpaceLoop
			jmp gameInit
			
;************************************************
;*				GAME ROUTINES					*
;************************************************

;#############DELAY LOOP#################
delayLoop:	ldx #0
			ldy gameSpeed
dlLp:		dex
			bne dlLp
			dey
			bne dlLp
			rts
;#############NEW ASTEROIDS###############
;Adds four big random asteroids to the game.
newAst:		pha
			txa
			pha
			
			ldx #4
			
naLoop:		;aType must be $10,$20,$30,$40
			jsr randNum
			and #%00110000
			bne naTSkip
			lda #$40
naTSkip:	sta aType
			
			;aVXY must be 1...14
			;Bits 1,0: 00:Nothing 01:X+ 10:X-
			;Bits 3,2: 00:Nothing 01:Y+ 10:Y-
naVXYnoVal:	jsr randNum
			and #%00001111
			beq naVXYnoVal
			cmp #15
			beq naVXYnoVal
			sta aVXY
			
			;Random position
			
			;X must be 0...24
naXnoVal:	jsr randNum
			and #%00011111
			cmp #25
			bcs naXnoVal
			sta aPosX
			
			;Y must be 0...15
			jsr randNum
			and #%00001111
			sta aPosY
			
			jsr addAst
			
			dex
			bne naLoop

			pla
			tax
			pla
			rts

;#############INCREMENT POINTS############
;Increments points in BCD
incPoints:	

			lda points
			
			cmp #$99
			beq ipIncHi
			
			clc
			sta tmp1
			and #%00001111
			cmp #9
			bcc ipAdd1 ;Less than 9, add 1
						; more, add 7 (BCD)
			lda tmp1
			adc #7
			jmp ipSkip1
			
ipAdd1:		lda tmp1
			adc #1
			
ipSkip1:	sta points 
			jmp ipEnd
			
ipIncHi:	inc lives ;Each 100 points add one live.
			lda #0
			sta points
			
			lda points + 1
			clc
			sta tmp1
			and #%00001111
			cmp #9
			bcc ipAdd2 ;Less than 9, add 1
						; more, add 7 (BCD)
			lda tmp1
			adc #7
			jmp ipSkip2
			
ipAdd2:		lda tmp1
			adc #1
			
ipSkip2:	sta points+1
	
ipEnd:		rts
;#############IMPACT HANDLER##############
;Checks when an asteroid is hit by a projectile
;and creates/deletes asteroids.
			
impHandler:
			lda pSpeed
			and #%11110000
			beq ihP2 ;P1 not active, check 2
			ldx p1X
			ldy p1Y
			jsr checkImpact
			bcc ihP2 ;No impact, check P2
			
			;If impact:
			
			lda pSpeed ;Disable projectile 1
			and #%00001111
			sta pSpeed
			
ihImpact:	jsr hideAst ;Hide asteroid

			txa
			asl a
			asl a
			tay
			
			;Check asteroid kind
			;aType, aPosX, aPosY, aVXY
			
			lda astList,y
			cmp #$10
			bcc ihSmall ;If small, remove it
			
			;Else it was big
			
			;Add 1 point
			jsr incPoints
			
			;If UFO, add another
			cmp #$30
			bne ihUFOSkip
			jsr incPoints
			
			; Replace by small
ihUFOSkip:	jsr randNum ;Random number 1...4, 4 small asteroid to choose from.
			and #%00000011
			clc
			adc #1
			sta astList,y
			
			; Add another small one, same position, different speed.
			iny 
			lda astList,y
			sta aPosX
			iny 
			lda astList,y
			sta aPosY
			
			;Bits 0,1: 00:Nothing 01:X+ 10:X-
			;Bits 2,3: 00:Nothing 01:Y+ 10:Y-
			iny 
			lda astList,y
			eor #$ff ;Invert speed
			sta aVXY
			
			jsr randNum ;Random number 0...3, 4 small asteroid to choose from.
			and #%00000011
			sta aType
			
			jsr addAst
			
			jmp ihP2
			
			
ihSmall:	jsr remAst

			;Add 3 points
			jsr incPoints
			jsr incPoints
			jsr incPoints

ihP2:		lda pSpeed
			and #%00001111
			beq ihEnd ;P2 not active, end.
			
			ldx p2X
			ldy p2Y
			jsr checkImpact
			bcc ihEnd ;No impact
			
			;If impact:
			lda pSpeed ;Disable projectile 2
			and #%11110000
			sta pSpeed
			
			jmp ihImpact
			
ihEnd:		rts
			
			
;##########RANDOM NUMBERS#############
;https://codebase64.org/doku.php?id=base:small_fast_8-bit_prng
;Generates a pseudo-random number 0-255, in rnSeed, also in A.

randNum:	lda rnSeed
			beq rnDoEor
			asl a
			beq rnNoEor ;if the input was $80, skip the EOR
			bcc rnNoEor
rnDoEor:    eor #$1d
rnNoEor:  	sta rnSeed
			rts
			
;##########CHECK IMPACT###############
;Projectile/ship coordinates in X,Y
;Impact: carry set, no impact: carry clear.
;If impact, index of asteroid in X
checkImpact:pha
			stx tmp1
			sty tmp2
			
			;Check for impact
			;astlist: aType, aPosX, aPosY, aVXY
			ldx #$FF
ciLoop:		inx
			cpx	nextAst
			beq ciNoImp;reached end of list, impact not found
			
			txa
			asl a
			asl a
			tay
			
			lda astList,y ; aType
			sta aType
			iny
			lda astList,y ; aPosX
			sta aPosX
			iny
			lda astList,y ; aPosY
			sta aPosY
			
			lda aType
			cmp #$10
			bcs ciBig
			
			lda aPosX
			cmp tmp1 ;pX 
			bne ciLoop
			lda aPosY
			cmp tmp2 ;pY
			bne ciLoop
			beq ciImp
			
ciBig:		ldy aPosX
			cpy tmp1 ;pX
			beq ciBY
			iny
			cpy tmp1 ;pX
			bne ciLoop
			
ciBY:		ldy aPosY
			cpy tmp2 ;pY
			beq ciImp
			iny
			cpy tmp2 ;pY
			bne ciLoop
			
ciImp:		sec
			bcs ciEnd
			
ciNoImp:	clc

ciEnd:		pla
			rts
			
			
			
;###########HIDE PROJECTILES############
hideProj:	pha

			lda #hi blankGraph
			sta gcAddr + 1
			lda #lo blankGraph
			sta gcAddr
			
			lda pSpeed ;Check is projectile is active.
			and #%11110000
			beq hpP2
			
			lda p1X
			sta textHIdx
			lda p1Y
			sta textVIdx
			jsr printGraph
			
hpP2:		lda pSpeed ;Check is projectile is active.
			and #%00001111
			beq hpEnd
			
			lda p2X
			sta textHIdx
			lda p2Y
			sta textVIdx
			jsr printGraph
			
hpEnd:		pla
			rts
			


;###########SHOW PROJECTILES############
showProj:	pha

			lda #hi projGraph
			sta gcAddr + 1
			lda #lo projGraph
			sta gcAddr
			
			lda pSpeed ;Check is projectile is active.
			and #%11110000
			beq spP2
			
			lda p1X
			sta textHIdx
			lda p1Y
			sta textVIdx
			jsr printGraph

spP2:		lda pSpeed ;Check is projectile is active.
			and #%00001111
			beq spEnd
			
			lda p2X
			sta textHIdx
			lda p2Y
			sta textVIdx
			jsr printGraph
			
spEnd:		pla
			rts
			
;############MOVE PROJECTILES###########
moveProj:	pha
			
			; pSpeed
			; Bits 7,6: 00:Nothing 01:X+ 10:X- ;1
			; Bits 5,4: 00:Nothing 01:Y+ 10:Y-
			
			; Bits 3,2: 00:Nothing 01:X+ 10:X- ;2
			; Bits 1,0: 00:Nothing 01:Y+ 10:Y-
			
			;Projectile 1:
			lda pSpeed
			and #%11110000
			beq mpInP2
			
			lda pSpeed
			and #%01000000
			bne mPInX1
			lda pSpeed 
			and #%10000000
			beq mPChY1
			
			dec p1X
			dec p1X
mPInX1:		inc p1X
			
mPChY1:		lda pSpeed
			and #%00010000
			bne mPInY1
			lda pSpeed
			and #%00100000
			beq mpInP2
			
			dec p1Y
			dec p1Y
mPInY1:		inc p1Y	
			
			;Projectile 2:
mpInP2:		lda pSpeed
			and #%00001111
			beq mpCol 
			
			lda pSpeed
			and #%00000100
			bne mPInX2
			lda pSpeed
			and #%00001000
			beq mpChY2
			
			dec p2X
			dec p2X
mPInX2:		inc p2X
			
mpChY2:		lda pSpeed
			and #%00000001
			bne mPInY2
			lda pSpeed
			and #%00000010
			beq mpCol
			
			dec p2Y
			dec p2Y
mPInY2:		inc p2Y
			
mpCol:		;First projectile
			lda p1X
			bmi mpColP1E
			cmp #25
			bcs mpColP1E
			
			lda p1Y
			bmi mpColP1E
			cmp #16
			bcs mpColP1E
			bcc mpColP2 ;Always taken
			
mpColP1E:	lda pSpeed
			and #%00001111
			sta pSpeed
			
			;Second projectile
mpColP2:	lda p2X
			bmi mpColP2E
			cmp #25
			bcs mpColP2E
			
			lda p2Y
			bmi mpColP2E
			cmp #16
			bcs mpColP2E
			bcc mpEnd ;Always taken
			
mpColP2E:	lda pSpeed
			and #%11110000		
			sta pSpeed
			
			
mpEnd:		pla
			rts
			
;############FIRE PROJECTILE############
fireProj:	pha

			; pSpeed
			; Bits 7,6: 00:Nothing 01:X+ 10:X- ;1
			; Bits 5,4: 00:Nothing 01:Y+ 10:Y-
			
			; Bits 3,2: 00:Nothing 01:X+ 10:X- ;2
			; Bits 1,0: 00:Nothing 01:Y+ 10:Y-
			
			;                 X Y
			; 0  N  %00000000 0 +
			; 8  NE %00001000 + +
			; 16 E  %00010000 + 0
			; 24 SE %00011000 + -
			; 32 S  %00100000 0 -
			; 40 SW %00101000 - -
			; 48 W  %00110000 - 0
			; 56 NW %00111000 - +
			
			
			lda shipRot
			lsr a
			lsr a
			lsr a
			
			tax
		
			;Projectile 1
			lda pSpeed
			and #%11110000 ;Pr 1 available
			bne fpChP2
			
			lda pSpeed
			and #%00001111
			sta pSpeed
			lda prSpTable,x
			asl a
			asl a
			asl a
			asl a
			ora pSpeed
			sta pSpeed
			
			lda shipX
			lsr a ;Actual ship position is shipX/4
			lsr a
			sta p1X
			lda shipY
			lsr a
			lsr a
			sta p1Y
			
			jmp fpEnd
			
			;Projectile 2
fpChP2:		lda pSpeed
			and #%00001111 ;Pr 2 available
			bne fpEnd
			
			lda pSpeed 
			and #%11110000
			ora prSpTable,x
			sta pSpeed
			
			lda shipX
			lsr a
			lsr a
			sta p2X
			lda shipY
			lsr a
			lsr a
			sta p2Y

fpEnd:		pla
			rts
			
			;Holds direction of projectile according to where the ship is pointing.
			;   76543210
prSpTable:	db %00000010, %00000110, %00000100, %00000101, %00000001, %00001001, %00001000, %00001010



			
;############CONTROL SHIP##########
;Left and righ arrow keys rotate ship.
;shipRot is index from shipGraph.

controlShip:pha
			jsr basicIn
			
			cmp #'8' ;Front arrow key.
			beq csThr
			jsr shipFric ;Simulate friction when not driving it
			cmp #'6' ;Right arrow key.
			beq csRR
			cmp #'4' ;Left arrow key.
			beq csRL
			cmp #' ' ;Space key fires projectiles.
			bne csNC
			jsr fireProj
			
csNC:		jmp csEnd
			
			;Rotate right
csRR		lda shipRot
			clc
			adc #8			
			cmp #57
			bcc csSkip1
			lda #0
csSkip1:	sta shipRot
			jmp csEnd
			
			;Rotate left
csRL		lda shipRot
			sec
			sbc #8			
			cmp #200
			bcc csSkip2
			lda #56
csSkip2:	sta shipRot
			jmp csEnd
			
			;                 X Y
			; 0  N  %00000000 0 +
			; 8  NE %00001000 + +
			; 16 E  %00010000 + 0
			; 24 SE %00011000 + -
			; 32 S  %00100000 0 -
			; 40 SW %00101000 - -
			; 48 W  %00110000 - 0
			; 56 NW %00111000 - +

			; shipVX -8 to +8 Actual speed is +-4, divided later
			; shipVY -8 to +8
			
			;Thrust. Increase speed in direction it's pointing.
csThr:		lda shipRot
			cmp #8
			beq csTInX
			cmp #16
			beq csTInX
			cmp #24
			beq csTInX
			
			cmp #40
			beq csTDecX
			cmp #48
			beq csTDecX
			cmp #56
			beq csTDecX
			bne csTCheckY
			
			
csTInX:		lda shipVX
			bmi csTIXSkip
			cmp #8
			bcs csTCheckY
csTIXSkip:	inc shipVX
			inc shipVX
			jmp csTCheckY
			
csTDecX:	lda shipVX
			bpl csTDXSkip
			cmp #$F9 ; -7
			bcc csTCheckY
csTDXSkip:	dec shipVX
			dec shipVX
			jmp csTCheckY
			
csTCheckY:	lda shipRot ;Check these first, unchecked by X
			cmp #0 
			beq csTDecY
			cmp #32
			beq csTInY
			
			cmp #8
			beq csTDecY
			cmp #56
			beq csTDecY
			cmp #24
			beq csTInY
			cmp #40
			beq csTInY
			bne csEnd
			
csTInY:		lda shipVY
			bmi csTIYSkip
			cmp #8
			bcs csEnd
csTIYSkip:	inc shipVY
			inc shipVY
			jmp csEnd
			
csTDecY:	lda shipVY
			bpl csTDYSkip
			cmp #$F9 ; -7
			bcc csEnd
csTDYSkip:	dec shipVY
			dec shipVY

csEnd:		
			pla
			rts

;#############MOVE SHIP#############
;Move ship according to its speed.
;Actual ship position is shipX/4, shipY/4
;Decrease speed as if simulating "friction"
moveShip:	pha

			;Check for impact with asteroids
			lda shipX
			lsr a
			lsr a
			tax
			lda shipY
			lsr a
			lsr a
			tay
			jsr checkImpact
			bcc msNoImp ;No impact, go on.
			
			;Impact
			dec lives ;Decrement lives.
			
			lda #0		;Set speed to 0
			sta shipVX
			sta shipVY
			
			;Random new position. 
			jsr randNum
			and #%01111111 ;Mask so we get 0...127->0...31
			sta shipX      ;  numbers over 25 wrap around the screen.
			
			jsr randNum
			and #%00111111;Mask so we get 0...63->0...15
			sta shipY
			
msNoImp:			
			;Add X speed, divided by 2 with sign extension.
			clc
			lda shipVX
			bpl msVXPlus
			sec
msVXPlus:	ror a
			clc
			adc shipX
			
			;Wrap around screen boundaries
			bpl msSkip1
			lda #%01100100 ;25*4
			sta shipX
			bne msAdjY
msSkip1:	cmp #%01100100 ;25*4
			bcc msSkip2
			lda #%00000000
msSkip2:	sta shipX
				
			;Add Y speed, divided by 2 with sign extension.
			clc
msAdjY:		lda shipVY
			bpl msVYPlus
			sec
msVYPlus:	ror a
			clc
			adc shipY
			
			;Wrap around screen boundaries
			bpl msSkip3
			lda #%01000000 ;16*4
			sta shipY
			bne msEnd
msSkip3:	cmp #%01000000 ;16*4
			bcc msSkip4
			lda #%00000000
msSkip4:	sta shipY
			
msEnd:		pla
			rts
			
;###########SHIP FRICTION########
shipFric:	;Friction simulation
			pha
			lda shipVX
			beq sfFrY
			bmi sfInVX
			dec shipVX
			dec shipVX
sfInVX:		inc shipVX

sfFrY:		lda shipVY
			beq sfEnd
			bmi sfInVY
			dec shipVY
			dec shipVY
sfInVY:		inc shipVY
sfEnd:		pla
			rts
			
;#############HIDE SHIP#############
hideShip:	pha
			lda shipX
			lsr a ;Divide by 4 (More fine control of speed)
			lsr a
			sta textHIdx
			lda shipY
			lsr a ;Divide by 4 (More fine control of speed)
			lsr a
			sta textVIdx
			
			lda #hi blankGraph
			sta gcAddr + 1
			lda #lo blankGraph
			sta gcAddr
			
			jsr printGraph
			pla
			rts
			
;############SHOW SHIP############
showShip:	pha
			lda shipX
			lsr a ;Divide by 4 (More fine control of speed)
			lsr a
			sta textHIdx
			lda shipY
			lsr a ;Divide by 4
			lsr a
			sta textVIdx
			
			lda #hi shipGraph
			sta gcAddr + 1
			lda #lo shipGraph
			clc
			adc shipRot
			sta gcAddr 
			bcc ssSkip
			inc gcAddr + 1
			
ssSkip:		jsr printGraph
			pla
			rts
			
			
			

			
;#############MOVE ASTEROID############
;Moves asteroid (index X) according to aVXY
;Bits 1,0: 00:Nothing 01:X+ 10:X-
;Bits 3,2: 00:Nothing 01:Y+ 10:Y-

moveAst:	pha
			tya
			pha
			txa
			pha
			
			;Multiply index by 4.
			txa
			asl a
			asl a
			tax
			
			lda astList+1,x
			sta aPosX
			lda astList+2,x
			sta aPosY
	 
			lda astList+3,x ;aVXY
			and #%00000001
			bne maXPos
			lda astList+3,x ;aVXY
			and #%00000010
			bne maXNeg
			beq maXSkip ;Always taken
			
maXPos		inc aPosX
			inc aPosX
maXNeg		dec aPosX
maXSkip		
			lda astList+3,x ;aVXY
			and #%00000100
			bne maYPos
			lda astList+3,x ;aVXY
			and #%00001000
			bne maYNeg
			beq maYSkip ;Always taken
			
maYPos		inc aPosY
			inc aPosY
maYNeg		dec aPosY
maYSkip

			;Wrap around screen boundaries.
			lda aPosX
			cmp #254
			bne maXAdj1
			lda #25
			bne maXAdjEnd ;Always taken
maXAdj1:	cmp #25
			bne maXAdjEnd
			lda #254
maXAdjEnd:  sta astList+1,x

			lda aPosY
			cmp #254
			bne maYAdj1
			lda #16
			bne maYAdjEnd ;Always taken
maYAdj1:	cmp #16
			bne maYAdjEnd
			lda #254
			
			
maYAdjEnd:	sta astList+2,x
			
			pla
			tax
			pla
			tay
			pla
			rts
			
;#############ADD ASTEROID#############
;Adds asteroid with param aType, aPosX, aPosY, aVXY
;to asteroid list.
addAst:		pha
			txa
			pha
			tya
			pha

			ldx #0
			lda nextAst
			asl a
			asl a
			tay
			
aaLoop:		lda aType,x
			sta astList,y
			inx
			iny
			cpx #4
			bcc aaLoop
			
			inc nextAst

			pla
			tay
			pla
			tax
			pla
			rts
			
;############REMOVE ASTEROID#############
;Removes asteroid with X index from asteroid list,
;fills void with last one if present.

remAst:		pha
			tya
			pha
			txa
			pha
			
			;Multiply index by 4.
			txa
			asl a
			asl a
			tax
			
			lda nextAst
			beq raEnd ;If 0 no asteroid to remove
			dec nextAst
			lda nextAst
			beq raEnd ;If one, removed by decrementing nextAst
			
			;Else, copy last one in place of the one being erased.
			;If removing the last one, it just copies itself to itself.
			;Multiply index by 4.
			asl a
			asl a
			tay
			
			lda astList,y
			sta astList,x
			lda astList+1,y
			sta astList+1,x
			lda astList+2,y
			sta astList+2,x
			lda astList+3,y
			sta astList+3,x
			
			
raEnd:		pla
			tax
			pla
			tay
			pla
			rts
			

;############SHOW ASTEROID###############
;Shows an asteroid with index X 
;Type: $10: Big A, $20:Big B, else small C,D,E,F 
showAst:	pha
			tya
			pha
			txa
			pha
			
			;Multiply index by 4.
			txa
			asl a
			asl a
			tax
			
			lda astList+1,x ;aPosX
			sta textHIdx
			lda astList+2,x ;aPosY
			sta textVIdx
			
			lda astList,x ;Type
			cmp #$10
			beq saA
			cmp #$20
			beq saB
			cmp #$30
			beq saG
			cmp #$40 ;Undefined, repeats $10.
			beq saA
			cmp #$1
			beq saC
			cmp #$2
			beq saD
			cmp #$3
			beq saE
			cmp #$4
			beq saF
			
			jmp saEnd
			
			
saA:		lda #hi astAGraph
			sta gcAddr + 1
			lda #lo astAGraph
			sta gcAddr
			jsr printGraph
			jmp saBig

saB:		lda #hi astBGraph
			sta gcAddr + 1
			lda #lo astBGraph
			sta gcAddr
			jsr printGraph
			jmp saBig
			
saG:		lda #hi ufoGraph
			sta gcAddr + 1
			lda #lo ufoGraph
			sta gcAddr
			jsr printGraph
			jmp saBig
			
saC:		lda #hi fragGraph
			sta gcAddr + 1
			lda #lo fragGraph
			sta gcAddr
			jmp saPrEnd
saD:		lda #hi (fragGraph+8)
			sta gcAddr + 1
			lda #lo (fragGraph+8)
			sta gcAddr
			jmp saPrEnd
saE:		lda #hi (fragGraph+16)
			sta gcAddr + 1
			lda #lo (fragGraph+16)
			sta gcAddr
			jmp saPrEnd
saF:		lda #hi (fragGraph+24)
			sta gcAddr + 1
			lda #lo (fragGraph+24)
			sta gcAddr
			jmp saPrEnd
			
saBig:		lda gcAddr
			clc
			adc #8
			sta gcAddr
			bcc saA1
			inc gcAddr + 1
saA1		inc textHIdx
			jsr printGraph
			
			clc
			adc #8
			sta gcAddr
			bcc saA2
			inc gcAddr + 1
saA2		inc textVIdx
			jsr printGraph
			
			clc
			adc #8
			sta gcAddr
			bcc saA3
			inc gcAddr + 1
saA3		dec textHIdx
saPrEnd:	jsr printGraph				
saEnd:		pla
			tax
			pla
			tay
			pla
			rts

;############HIDE ASTEROID###############
;Hides an asteroid with index X 
;Type: $10: Big A, $20:Big B, else small 0...3 
hideAst:	pha
			tya
			pha
			txa
			pha
			
			;Multiply index by 4.
			txa
			asl a
			asl	a
			tax
			
			;Whitespace to erase asteroid
			lda #hi blankGraph
			sta gcAddr + 1
			lda #lo blankGraph
			sta gcAddr
			
			;Erase top left, always needed
			lda astList+1,x ;aPosX
			sta textHIdx
			lda astList+2,x ;aPosY
			sta textVIdx
			jsr printGraph
			
			lda astList,x ;Type
			
			cmp #$10
			bcc haSmall
			
			;Erase top right, and bottom left-right if big.
			inc textHIdx
			jsr printGraph
			inc textVIdx
			jsr printGraph
			dec textHIdx
			jsr printGraph
			dec textVIdx
			
haSmall:	pla
			tax
			pla
			tay
			pla
			rts
			

;###########PRINT GRAPH############
;Prints "graphical" char at textHIdx, textVIdx
;Character defined at gcAddr			
printGraph:	pha
			txa
			pha
			tya
			pha
			
			;HI VRAM INDEX
			lda textVIdx
			cmp #16
			bcs pgEnd
			ora #hi vRAMbase
			sta vRAMIdx +1
			
			;LOW VRAM INDEX
			lda textHIdx
			cmp #25
			bcs pgEnd
			sta vRAMIdx
			
			;Copy the 8 bytes that form a character from ROM to vRAM
			ldy #$0	;Memory index
			sty tmp1;ROM Index
			ldx #$0 ;vRAM Index
			
pgPrLoop:	ldy tmp1
			inc tmp1
			
			lda (gcAddr),y
			stx tmp2 ;There is no txy instruction available :(
			ldy tmp2
			sta (vRAMIdx),y
	
			txa
			clc
			adc #$20 ;Add $20 to vRAM index -> Next line
			tax
			bne pgPrLoop ;Last addition: $E0 + $20 = $00
			
pgEnd:		pla
			tay
			pla
			tax
			pla
			rts
			
			
			
shipGraph:	db  $00, $10, $10, $10, $38, $38, $7C, $00 ; $0
			db	$00, $02, $0C, $3C, $18, $08, $00, $00 ; $1
			db	$00, $40, $70, $7E, $70, $40, $00, $00 ; $2
			db	$00, $00, $08, $18, $3C, $0C, $02, $00 ; $3
			db	$00, $7C, $38, $38, $10, $10, $10, $00 ; $4
			db	$00, $00, $10, $18, $3C, $30, $40, $00 ; $5
			db	$00, $00, $02, $0E, $7E, $0E, $02, $00 ; $6
			db	$00, $40, $30, $3C, $18, $10, $00, $00 ; $7
			
astAGraph:	db  $00, $01, $0F, $3F, $3F, $1F, $0F, $0F ; $0
			db	$00, $80, $F0, $F0, $E0, $E0, $F8, $FC ; $1
			db	$FC, $FC, $F8, $E0, $E0, $00, $00, $00 ; $2
			db	$0F, $1F, $3F, $3F, $18, $00, $00, $00 ; $3
			
astBGraph:	db  $00, $00, $00, $00, $03, $03, $1F, $3F ; $0
			db	$00, $00, $30, $F8, $F8, $FC, $FC, $F8 ; $1
			db	$F8, $F8, $F0, $E0, $E0, $00, $00, $00 ; $2
			db	$3F, $7F, $7F, $3F, $3F, $1E, $0C, $00 ; $3
			
			
fragGraph:	db  $00, $00, $38, $78, $7C, $3C, $38, $00 ; $0
			db	$00, $00, $18, $3C, $3C, $08, $00, $00 ; $1
			db	$00, $00, $00, $30, $7C, $78, $00, $00 ; $2
			db	$00, $00, $18, $3C, $38, $18, $00, $00 ; $3

blankGraph:	db	$00, $00, $00, $00, $00, $00, $00, $00 ; Blank

ufoGraph:	db  $00, $00, $03, $04, $04, $1F, $20, $40 ; $0
			db	$00, $00, $C0, $20, $20, $F8, $04, $02 ; $1
			db	$FF, $02, $04, $F8, $00, $00, $00, $00 ; $2
			db	$FF, $40, $20, $1F, $00, $00, $00, $00 ; $3
			
projGraph:	db	$00, $00, $00, $00, $10, $00, $00, $00 ; Projectile

			
			
.end