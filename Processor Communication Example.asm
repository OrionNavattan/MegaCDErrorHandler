; -------------------------------------------------------------------------
; This file contains several code samples from Sonic CD that have been 
; modified as examples of how to check the sub CPU's status, and enter the 
; error handler if it has crashed. Exactly how you implement it will depend
; upon your interprocessor communication protocol, but in all cases you 
; should check for the sentinel value (in this case, $FF written to 
; mcd_sub_flag) before sending a command to the sub CPU, giving the wordram
; to the sub CPU, and at each VBlank before triggering the MD interrupt.
; It is also advised to incorporate a check into the wait loops for 
; processing a command and waiting for wordram access.
; -------------------------------------------------------------------------

checksubCPU:	macro dest
		cmpi.b	$FF,(mcd_sub_flag).l		; has sub CPU crashed?
		beq.w	\dest				; branch if so
		endm

; -------------------------------------------------------------------------
; Send a command to the sub CPU
; -------------------------------------------------------------------------

SubCPUCommand:
		checksubCPU SubCPUCrash
		move.w	d0,(mcd_maincom_0).l			; send the command

	.wait_subCPU:
		checksubCPU SubCPUCrash
		move.w	(mcd_subcom_0).l,d0			; has the sub CPU received the command?
		beq.s	.wait_subCPU			; if not, wait
		cmp.w	(mcd_subcom_0).l,d0		; is it processing the current command?
		bne.s	.wait_subCPU			; if not, wait

		move.w	#0,(mcd_maincom_0).l			; mark as ready to send commands again

	.wait_subCPU2:
		checksubCPU SubCPUCrash
		move.w	(mcd_subcom_0).l,d0			; is the sub CPU done processing the command?
		bne.s	.wait_subCPU2			; if not, wait
		rts		
		
; -------------------------------------------------------------------------
; Wait for Word RAM access
; -------------------------------------------------------------------------

WaitWordRAMAccess:
		checksubCPU	SubCPUCrash
		btst	#0,GAMEMMODE			; do we have Word RAM access?
		beq.s	WaitWordRAMAccess		; if not, wait
		rts

; -------------------------------------------------------------------------
; Give Sub CPU Word RAM access
; -------------------------------------------------------------------------

GiveWordRAMAccess:
		checksubCPU	SubCPUCrash
		bset	#1,GAMEMMODE			; give Sub CPU Word RAM access
		beq.s	GiveWordRAMAccess		; branch if it has not been given
		rts		


; -------------------------------------------------------------------------
; If sub CPU has crashed
; -------------------------------------------------------------------------

SubCPUCrash:
		trap #0	; enter the sub CPU error handler