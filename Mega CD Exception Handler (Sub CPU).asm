AddressError:
		__ErrorMessage "SUB CPU: ADDRESS ERROR", _eh_show_sr_usp|_eh_address_error

IllegalInstr:
		__ErrorMessage "SUB CPU: ILLEGAL INSTRUCTION", _eh_show_sr_usp

ZeroDivide:
		__ErrorMessage "SUB CPU: ZERO DIVIDE", _eh_show_sr_usp

ChkInstr:
		__ErrorMessage "SUB CPU: CHK INSTRUCTION", _eh_show_sr_usp

TrapvInstr:
		__ErrorMessage "SUB CPU: TRAPV INSTRUCTION", _eh_show_sr_usp

PrivilegeViol:
		__ErrorMessage "SUB CPU: PRIVILEGE VIOLATION", _eh_show_sr_usp

Trace:
		__ErrorMessage "SUB CPU: TRACE", _eh_show_sr_usp

Line1010Emu:
		__ErrorMessage "SUB CPU: LINE 1010 EMULATOR", _eh_show_sr_usp

Line1111Emu:
		__ErrorMessage "SUB CPU: LINE 1111 EMULATOR", _eh_show_sr_usp

ErrorExcept:
		__ErrorMessage "SUB CPU: ERROR EXCEPTION", _eh_show_sr_usp

pcm_ctrl:		equ	$FF000F

MainCPUError:
		; If sub CPU symbol table support is enabled, we need to terminate hardware ops
		; and give the wordram to the main CPU so its symbol table can be decompressed.
		move	#$2700,sr				; disable interrupts
		st.b	(mcd_sub_flag).w			; set flag to let main CPU know we've noticed
		bsr.s	TerminateSubCPUOps		; terminate sub CPU hardware operations
		bra.s	SubCPU_Done
; ===========================================================================

ErrorHandler:
		move	#$2700,sr				; disable interrupts
		st.b (mcd_sub_flag).w		; set flag to let main CPU know we've crashed (assumes communication protocol includes checking this flag for $FF before sending commands or while waiting for responses)
		movem.l	d0-a6,-(sp)				; dump all registers
		move.l	usp,a0
		move.l	a0,-(sp)			; dump USP (unnecessary if BIOS is being used, as user mode can not be used with it)
		move.w	#BIOSStatus,d0
		jsr	(_CDBIOS).w				; get BIOS status for display by console app
		bsr.s	TerminateSubCPUOps		; terminate sub CPU hardware operations
		move.l	sp,(mcd_subcom_0).w	; get address of bottom of stack (including dumped registers) for main CPU

SubCPU_Done:
		clr.b	(mcd_sub_flag).w ; clear flag to let main CPU know we are done
		bra.s	*	; stay here forever
; ===========================================================================

TerminateSubCPUOps:
		moveq	#0,d0
		move.w  d0,(cdd_fader).w	; silence CDDA
		move.b	d0,(pcm_ctrl).l		; silence the RF5C164 (cannot use clr as this is a write-only register)

		move.w	#SubcodeStop,d0
		jsr (_CDBIOS).w				; terminate subcode operations if running (does nothing if not already running)

		move.w	#DecoderStop,d0
		jsr (_CDBIOS).w				; terminate decoder operations if running (does nothing if not already running)

		move.b	#red_led,(led_control).w	; power LED off, access LED on
		bclr	#peripheral_reset_bit,(mcd_reset).w	; stop disc drive by reseting it

	.waitgfx:
		tst.b	(gfx_op_flag).w	; is a GFX operation in progress?
		bne.s	.waitgfx		; if so, wait for it to finish

	.waitmain:
		cmpi.b	#$FF,(mcd_main_flag).w	; has the main CPU noticed?
		bne.s	.waitmain	; if not, branch

		; Main CPU has noticed
		lea (mcd_mem_mode).w,a0

	.wait2M:
		andi.b	#~(priority_underwrite|priority_overwrite),(a0)	; disable priority mode
		bclr	#wordram_mode_bit,(a0)	; set wordram to 2M mode
		bne.s	.wait2M						; branch if it wasn't already in 2M mode (wait for config change to complete)


		btst	#wordram_swapsub_bit,(a0)	; is wordram assigned to main CPU?
		beq.s	.done						; branch if so

		bset	#wordram_swapmain_bit,(a0)	; give wordram to main CPU

	.done:
		rts
