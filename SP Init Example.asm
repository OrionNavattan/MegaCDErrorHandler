; ---------------------------------------------------------------
; If the BIOS is used, the user init routine of the sub CPU 
; program will need to set up the jump table entries for the 
; exception vectors. The following is an example of how to do this.
; ---------------------------------------------------------------
		
SPInit:		
		lea ExceptionPointers(pc),a0 ; pointers to exception entry points
		lea _AddressError(pc),a1	; first error vector in jump table
		moveq	#9-1,d0			; 9 vectors total

	.vectorloop:
		addq.l	#2,a1		; skip over instruction word
		move.l	(a0)+,(a1)+	; set table entry to point to exception entry point
		dbf d0,.vectorloop	; repeat for all vectors
		rts
	
ExceptionPointers:
		dc.l AddressError
		dc.l IllegalInstr
		dc.l ZeroDivide
		dc.l ChkInstr
		dc.l TrapvInstr
		dc.l PrivilegeViol
		dc.l Trace
		dc.l Line1010Emu
		dc.l Line1111Emu