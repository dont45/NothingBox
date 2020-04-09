;
;***** Nothing Box *****
;Author:
;		Don Thompson
;		April 2020
;		COVID-19 Keep Busy Project
;
;Project:
;		Build a Box which shuts off any and all switches 
;		which you turn on in the exact order which you
;		switched them on.
;
;Version:
;		1.1.0
;Platform:
;		Atmel Mega16 processor
;		AVR Assembler
;
;NothingBox Contains:
;	 	1) Eight toggle switches
;    	2) A Hand which moves to a specific switch and shuts it off
;   	3) A Door which opens to give access to switches
;
;
;Software Routines:
;    	A) Constantly scan switches to detect any switched on
;    	Upon detecting an on switch, push to stack which is
;    	implemented as a first in - first out ring.
;    	B) Constantly scan stack to see if any entry present.
;    	C) If entry is found, hand shuts switch off
;    	Switch-Off Routines
;    	D) position hammer to desired switch
;	 	E) open hammer door
;	  	F) activate hammer to shut switch off
;	 	G) close hammer door
;
;Process Logis
;		Loop:
;		A) Read switch port
;		B) Process new Port - swin
;		   add all new switches, only add if not in prevsw
;		   AND with not prevsw: 
;		    new    prev  not p   result
;		   	0001 & 0110 ~1001 => 0001
;		C) shut off oldest switch on - shutsw
;		D) Debug: display switches still on - showsws
;
; ***** BUGS TO FIX:
;      
; *****

.include "m16def.inc"

.def	Temp	=r16
.def	Delay	=r17
.def	Delay2	=r18

;**** Define Registers and Storage
.DSEG
swbuf:  .BYTE 12	;reserve 8 + reserve bytes to push switch
testsw: .BYTE 1		;test rolling switch input
prevsw:	.BYTE 1		;previous read of switch port
cursw:	.BYTE 1		;simulate switches currently set in LEDs


.CSEG
;**** Initialization ****
; Initialize ports for IO

; Initialize all servos and calibrate ??

; Initialize registers

; Initialize Stack
		ldi	 r16,LOW(RAMEND)
		out  SPL,r16
		ldi	 r16, HIGH(RAMEND)
		out  SPH,r16

		ser	 Temp
		out	 DDRB,Temp			;set PORTB to output
		clr  r16
		out  PORTD,r16
		out  DDRD,r16
		call clrsw				;init X and Y and zero swbuf

; Debug test loop
Loop:	
;		The Magic of it all:
		call getsws			;add switches you've flipped
		call showsws		;debug: turn on LED's
;		call Dly
		call shutsw			;and  machine shuts them off!
		call Dly
		rjmp Loop

; subroutines

;display ring buf in LEDs
showsws:
		push r21
		ldi  r18,0
		out  PORTB,r18
show1:	
		ldi  r17,1			;peek in swout
		call swout			;don't remove it, just look
		cpi  r16,0
		breq show2
		call cvtbb			;shift: (1 << r16)
		or   r18,r16		;build bits set
		rjmp show1
show2:
		;store to cursw
		ldi	 r30,low(cursw)	;Load Z reg low
		clr  r31
		st	 Z,r18
		ldi  r21,0xff
		eor  r18,r21
		out  PORTB,r18
		pop  r21
		ret

;shut off ONE switches in ring, first in
shutsw:
		ldi  r17,0				;swout remove entry
		call swout				;get sw in r16
; ??? complete
		ret

;add all switches not prev switched ?
;bug: not checking prev entry ??
getsws:	push r21
		ldi  r16,1
 		in 	 r20,PIND			;read switches
		ldi  r21,0xff
		eor  r20,r21			;invert all sw bits 
		ldi	 r21,8				;loop 8 for bits
; ***********   DEBUG   *************
 		ldi  r20,0x15			;TEST VAlUE for SIM
		call gettest			;get next test sw values
		cpi	 r20,0				;skip if all zero
		brbc 1,addl1
		pop  r21
		ret						;skipping
addl1:	push r20				;carry shifted in, ok
		andi r20,0x01			;get one port bit 
		brbs 1,addl3			;skip call if zero
		call swin				;get switch input
addl3:  pop  r20
		ror  r20
addl2:	inc  r16
		dec	 r21
		brne addl1
		pop  r21
		ret

;check if swbuf enpty (X = Y?
isEmpty:
		ldi  r16,0
		cp   r26,r28
		brne empty
		inc  r16
empty:
		ret						;return true/false (1/0) in r0

;push sw into next ring position
swin:
		st   X+,r16
		push r31				;using Z
		ldi	 r31,low(swbuf)+7	;- Load Z reg low
		cp	 r26,r31			;check buf overflow
		brlt noof
		ldi	 r26,low(swbuf)		;wrap ring to beginnig
noof:	
		pop  r31	
		ret

;pop earliest entry from ring
;if r17 <> 0 then peek
;set r17 before call
swout:	
       	push r31
		ldi  r16,0
		cp 	 r26,r28
		breq swo1				;if X==Y no entry
		ld	 r16,Y				;get entry
		clr	 r0	
		cpi  r17,0
		brne swo1				
		st	 Y,r0				;and clear in ring
		ldi	 r31,low(swbuf)+7	;check if Y overflowed ring buffer;
		cp	 r28,r31
		brlt swo1				;no overflow
		ldi	 r28,low(swbuf)		;wrap ring out pointer to beginning
swo1:
		inc  r28				;move to Y+
		pop  r31
		ret

;debug routines

;set each entry in ring buf to zero
clrsw:
		call initbuf			;init ring to empty
		ldi   r16,14			; 12 byte buffer + some	
		ldi	  r18,0				;write 0 into buf entries
zzsw:
		cpi	  r16,0
		breq  zzd
	    st	  X+,r18
		st	  Y+,r18			;?only need to write once
		dec	  r16
		rjmp  zzsw
zzd:	call initbuf			;reset ring to empty
		ret

;initialize ring buf pointers, in and out
initbuf:
; initialize X and Y as in and out pointers to swbuf
		ldi	 r26,low(swbuf)		;swin  - Load X reg low
		clr  r27
		ldi	 r28,low(swbuf)		;swout - Load Y reg low
		clr  r29
		ret	

;convert binary to bit
;shift 1 left by r16 bits
;(1 << r16)
cvtbb:	push r20
		ldi  r20,1
		cpi  r16,9
		brge bb2				;do nothing if >8
bb1:
		lsl  r20
		dec  r16
		cpi  r16,0
		brne bb1
bb2:
		lsr  r20				;bit 7 gone?
		mov	 r16,r20
		pop  r20
		ret

;get a rolling switch input for testing in r16
gettest:
		push r31
		push r30
		ldi	 r30,low(testsw)	;testsw - Load Y reg low
		clr  r31
		ld   r20,Z
		inc	 r20
		st   Z,r20
		pop  r30
		pop  r31
		ret
Dly:
		push Delay
		push Delay2	
		dec  Delay
		brne Dly
		dec  Delay2
		brne Dly
		pop  Delay2
		pop  Delay
		ret
End:

