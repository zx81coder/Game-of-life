; Game of Life 
; a 1K sim

		org 	#4009

eoscr		equ	dfile+33*24+1	
; the first address after the screen is the end of the screen

; the relocated directionroutines
up		equ 	#4000
down		equ 	up-upc+downc
left		equ     up-upc+leftc
right		equ	up-upc+rightc
dir7tab		equ	up-upc+dir7tabc
scrlp		equ	up-upc+scrlpc
scrlp1		equ	up-upc+scrlpc1
kfnd		equ	up-upc+kfndc

; testvalues to check copy over sysvar goes OK
dfileok		equ	up-upc+dfilec-basic-3
frameok		equ	up-upc+framesc-frames
lastkok		equ	up-upc+lastkc-lastk

; in LOWRES more sysvar are used, but in this way 
; the shortest code over sysvar to start 
; machinecode. This saves 11 bytes of BASIC

; DO NOT CHANGE AFTER BASIC+3 (=DFILE)
basic   	ld 	h,dfile/256		; highbyte of dfile
        	jr 	init1

		db 	236			; BASIC over DFILE data
		db 	212,28			; GOTO USR 0
; short FP notation for #8009, no NEWLINE for BASIC
		db 	126,143,0,18	

eline		dw 	last
chadd		dw 	last-1
		db 	0,0,0,0,0,0	
berg		db 	0			
mem		db 	0,0		
init1		ld 	l, dfile mod 256	; low byte of dfile
		jr 	init2			
lastk		db 	255,255,255
margin  	db 	55
nxtlin  	dw 	basic			; goto BASIC-line over sysvar	
flagx   	equ 	init2+2

init2		ld 	(basic+3),hl		; repair DFILE sysvar
		ld	h,eoscr/256
		db 	0,0		
		ld	l,eoscr mod 256
frames  	db 	#37			; LOAD will do 1 DEC on this address
		db	#e9			; set end of screen
		xor	a	
		ex	af,af'			; delay intrupt
		jp 	init			; initroutine
cdflag  	db 	64

; DO NOT CHANGE SYSVAR ABOVE 
; UNLESS YOU KNOW WHAT YOU DO
	
		ld	h,dfile / 256		; HL now DFILE, part is set over sysvar
nfld		inc	hl
		ld	a,(hl)			; get screenposition
		cp	#76			; test end of line
		jr	z,nfld			; skip if so
		cp	#e9			; test end of screen marker	
		ret	z			; end of screen reached	
dowhat		call	count			; modified code, counter or set
		jr	nfld			; do all fields

keytab						; PORTS only to win bytes
		db	%11111110		; SH-V
		db 	fire mod 256

		db	%11111101		; A-G
		db 	down mod 256

		db	%11111011		; Q-T
		db 	up mod 256

		db	%11110111		; 1-5
		db 	left mod 256

		db	%11101111		; 6-0		
		db 	right mod 256

		db	%01111111		; B-space
		db	run mod 256

fire		ld	a,(de)			; get current field
		and	#d8			; take off counted fields from run
		xor	#d8			; swap status
		ld	(de),a			; set new status
;		ret				; exit through tabkey with invalid keyvalue saves a RET 

tabkey		ld	hl,keytab
fkey		cp	(hl)			; test current port
		inc	hl			; go to key pointer
		jr	z,kfnd			; matching port found
		inc	hl
		jr	c,fkey			; in decreasing order, so C is not found
		jr	right+5			; invalid key

run		pop	hl			; drop	return
		call	scrlp1			; count adjacent colonys and preset new
		ld	a,change mod 256
		call	scrlp			; kill overpopulated and fix set new 
		ld	a,(lastk)		; get lastkey pressed
		inc	a			; test ANY key
		jr	z,run+1			; do next generation when no key pressed

start		ld	l,c			; set cursor visible on screen
curloop		ld	e,(hl)			; save background
		ld	(hl),21			; show cursor

wup		inc	a			; test keystatus
wdown		ld	a,(lastk)		; get new keystatus
		jr	nz,wup			; wait for all keys up
		inc	a			; test keystatus
		jr	z,wdown			; wait for keydown

		ld	(hl),e			; repair background after keypress
		ld	d,h
		ld	e,l			; save original pointer

		dec	a			; undo change
		call	tabkey			; do the move	
		cp	#76			; test valid move
		jr	nz,curloop		; valid move
		ex	de,hl			; get old position
		jr	curloop			; stay in editor

change		add	a,a			; test field
		ret	c			; empty field no change
		set	3,(hl)			; make it a true colony	when in first stage
		ld	a,#59			; get colony border
		sub	(hl)			; 00 ff fe fd
		cp	256-2			; NC CC CC NC 
		ret	nc			; alive another round
		ld	(hl),#80		; kill colony, empty field
;		ret				; 00  or   FD so NEWCOL test does the RET

newcol		cp	#58+3			; test exactly 3
		ret	nz
		ld	(hl),#53		; a new colony is set, but not yet fully activated
		ret

count		push	hl			; save current position
		ld	de,#ffde		; -34
		add	hl,de			; now lefttop of field		
		ld	bc,dir7tab		; displacement table to all adjacent fields
		xor	a			; first displacement zero
		ld	d,#58			; counter now "zero" +indicator colony
bclp		add	a,l			; add displacement
		ld	l,a			; save new lowbyte
		adc	a,h			; >256, H added 
		sub	l			; but L subtracted
		ld	h,a			; H now new H
		call	right+5			; get next field data

		rra				; bit3 to bit2      1   0
		cpl				; invert bit 2      0   1
		or 	d			; 0 1 2 3           0   1
		and 	4
		jr	nz,nocnt		; no colony or d=4 DO NOT INCREASE ABOVE 4

		inc	d			; increase counter

nocnt		ld	a,(bc)			; get next field pointer
		inc	bc			; point to next loop
		rrca				; a=a/2
		jr	nc,bclp			; odd nr is exit			
endcnt		pop	hl			; get current field
		ld	a,(hl)			; get current fieldvalue
		add	a,a			; test field
		ld	a,d			; preload for NEWCOL
		jr	c,newcol		; empty cell can be activated

		ld	(hl),a			; set colony count 
		ret				; a colony is now ready


; the display file, Code the lines needed.
dfile 		db 	118
		block	32,#58			; 1 full line is copied 23x
		db	118

init		ld	de,65536-24*33		; -screensize
		ld	bc,65536-dfile		; -start of screen
		exx
		ld	hl,upc			; the routines to copy
		ld	de,#4000		; start of sysvar
		ld	bc,vars-upc 		; the length
		ldir				; copy routines

		ld	sp,#4400+1024		; +1K no effect in 1K, but better for my SP-2-ZX81 emulator 

		ld	hl,run+1		; start with a running screen
		push	hl			; set on stack

		ld	hl,dfile+1
		ld	de,init
		ld	bc,768+24-33
		jp	#a6e			; make full screen,copy 23 lines in ROM

; routines that are copied over sysvar so sysvar memory is used after loading
upc		ld	l,#df			; -33
		db	#3a			; LD A,(NN), skip LEFTC
leftc		ld	l,#ff			; -1
		ld	h,#ff			; also highbyte negative
		jr	move
		
kfndc		ld	l,(hl)			; get lowbyte keypressroutine
		jp	(hl)			; goto routine

		db	0			

dfilec		dw	dfile

downc		ld	l,33			; +33
		db	#3a			; again LD A,(NN) trick

rightc		ld	l,1			; +1
		ld	h,0			; high byte positive

move		add	hl,de			; HL now new field
		ld	a,#76			; preset false fieldvalue
		push	hl			; save new position

		exx
		pop	hl			; alternative register to test on screen
		add	hl,bc			; take off start of screen
		add	hl,de			; now test in screensize
		exx

		ret	c			; exit when out of screen with A=#76
		ld	a,(hl)			; get screenvalue, this could be end of line #76
		ret

		db	0,0,0,0			; a few bytes not used, not needed

lastkc		db	255,255,255		; LASTK
		db	55			; MARGIN

dir7tabc	db	2,2,62,4,62,2,2		; displacement table
		db	1			; endmarker table

scrlpc1		ld	a,count mod 256
		db	33			; skip FRAMES by setting HL

framesc		dw	65535			; FRAMES sysvar

scrlpc		ld	(dowhat+1),a		; set which routine to execute
		db	33			; LD HL,NN, set L and skip CDFLAG
		db	dfile mod 256

; the final NEEDED data for each program
vars    	db	128
last		equ $   	
end
