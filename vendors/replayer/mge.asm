	; asm file to create pt3.bin to use into mge
	output pt3.bin
	org 0xc000
	jp ayFX_SETUP                 ; +0
	jp ayFX_END                   ; +3
	jp ayFX_INIT                  ; +6
	jp ayFX_PLAY                  ; +9
	jp PT3_INIT                   ; +12
	jp PT3_ROUT                   ; +15
	jp PT3_PLAY                   ; +18
	include PT3-RAM.ASM
	include ayFX-RAM.ASM
	include PT3-ROM.ASM
	include ayFX-ROM.ASM
