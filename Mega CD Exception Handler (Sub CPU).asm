; ---------------------------------------------------------------
; Mega CD Error Handler - Sub CPU module
; ---------------------------------------------------------------
; Hardware and jump table addresses
; ---------------------------------------------------------------

_AddressError:	equ	$5F40	; _ADRERR; address error exception
_IllegalIns:	equ	$5F46	; _CODERR; illegal instruction exception
_DivZero:		equ	$5F4C	; _DEVERR; divide by zero exception
_ChkExcp:		equ	$5F52	; _CHKERR; CHK exception
_TrapV:			equ	$5F58	; _TRPERR; trap overflow exception
_PrivViol:		equ	$5F5E	; _SPVERR; privilege violation exception
_Trace:			equ	$5F64	; trace exception
_ALine:			equ	$5F6A	; _NOCOD0; A-line trap exception
_FLine:			equ	$5F70	; _NOCOD1; F-line trap exception	

program_ram:			equ 0

mcd_main_flag			equ $FFFF800E
mcd_sub_flag			equ	$FFFF800F
mcd_subcom_0			equ	$FFFF8020
mcd_subcom_2:			equ	$FFFF8024 

; ---------------------------------------------------------------
; Exception entry points on sub CPU. Identical to those used on
; main CPU with the exception of noting that they originated
; from the sub CPU. These are not pointed to directly by the
; vector table, but rather via the user jump table. Bus Error has 
; been removed, as there is no jump table entry for it.
; ---------------------------------------------------------------

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
		
; ---------------------------------------------------------------
; Sub CPU error handler module. About all it does is dump the
; registers and signal the main CPU that it has crashed. 
; Identical in both Mode 1 and 2. 

; Note that the system used here to inform the main CPU
; of the crash is an example; you can use any value and any of the 
; communication registers, so long as the main CPU checks for this
; flag before sending a command, at every VBlank, and before 
; waiting for wordram access. It is also recommended that this flag
; be checked as part of wait loops while waiting for wordram
; access or while waiting for commands to process.
; ---------------------------------------------------------------

ErrorHandler:
		disable_ints				; disable all interrupts
		st.b (mcd_sub_flag).w		; set flag to let main CPU know we've crashed (assumes communication protocol includes checking this flag for $FF before sending commands or while waiting for responses)
		movem.l	d0-a6,-(sp)				; dump all registers
		move.l	usp,a0
		move.l	a0,-(sp)			; dump USP (technically unnecessary if BIOS is being used, as user mode can not be used with it)
		
	.waitmain:
		cmpi.b	#$FF,(mcd_main_flag).w	; has the main CPU noticed?
		bne.s	.waitmain	; if not, branch
		
		; Main CPU has noticed
		move.l	sp,(mcd_subcom_0).w	; get address of bottom of stack (including dumped registers) for main CPU
		clr.b	(mcd_sub_flag).w ; clear flag to let main CPU know we are done
		bra.s	*	; stay here forever
