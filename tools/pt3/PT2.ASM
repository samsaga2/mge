;Universal PT2 player for ZX Spectrum and MSX computers
;(c)2005 S.V.Bulba <vorobey@mail.khstu.ru> http://bulba.at.kz

;Release number
Release: .equ '0'

;Improvements
;-max number of patterns: 85 (84 for PT2.4PF)
;-correct tone portamento (see Epilogue.pt2 by Nik-O)
;-note range checking like in PT3
;-supports all PT2 subversions including PT2.4PF
;-PT2.4PF modules can be loaded to any address
; (they converted to standard PT2 during INIT)

;Limitations
;-run in RAM only (self-modified code)
;-modules like GUEST4.pt2 by Alex Job (with incorrect
; note+ornament values) sound differ than with standard
; PT2-player

	;.ORG $C000

TonA:		.equ 0
TonB:		.equ 2
TonC:		.equ 4
Noise:	.equ 6
Mixer:	.equ 7
AmplA:	.equ 8
AmplB:	.equ 9
AmplC:	.equ 10
Env:		.equ 11
EnvTp:	.equ 13

;Entry and other points
;START initialization
;START+3 initialization with module address in HL
;START+5 play one quark
;START+8 mute
;START+10 setup and status flags
;START+11 pointer to current position value in PT2 module;
;After INIT (START+11) points to Postion0-1 (optimization)

START:
	LD HL,MDLADDR
	JR INIT
	JP PLAY
	JR MUTE
SETUP:	.db 0 ;set bit0 to 1, if you want to play without looping
	     ;bit7 is set each time, when loop point is passed
CrPsPtr	.dw 0 

;Identifier
	.db "=Uni PT2 Player r.",Release,"="

CHECKLP:	LD HL,SETUP
	SET 7,(HL)
	BIT 0,(HL)
	RET Z
	POP HL
	LD HL,DelyCnt
	INC (HL)
	LD HL,ChanA+CHP_SkpCnt
	INC (HL)
MUTE:	XOR A
	LD H,A
	LD L,A
	LD (AYREGS+AmplA),A
	LD (AYREGS+AmplB),HL
	JP ROUT_A0

INIT:
;HL - AddressOfModule

	LD (MODADDR),HL
	LD (MDADDR2),HL
	LD A,(HL)
	LD (Delay),A
	PUSH HL
	INC HL
	INC HL
	LD A,(HL)
	EX AF,AF'
	INC HL
	LD (SamPtrs),HL
	LD E,(HL)
	INC HL
	LD D,(HL)
	LD A,E
	OR D
	CALL NZ,PhF2PT
	LD DE,63
	ADD HL,DE
	LD (OrnPtrs),HL
	LD E,32
	ADD HL,DE
	LD C,(HL)
	INC HL
	LD B,(HL)
	LD E,30
	ADD HL,DE
	LD (CrPsPtr),HL
	INC HL
	EX AF,AF'
	LD E,A
	ADD HL,DE
	LD (LPosPtr),HL
	POP HL
	ADD HL,BC
	LD (PatsPtr),HL
	LD HL,VARS
	LD (HL),D
	LD DE,VARS+1
	LD BC,VAR0END-VARS-1
	LDIR
	LD (AdInPtA),HL ;ptr to zero
	
	LD HL,$F01 ;H - CHP_Volume, L - CHP_SkpCnt
	LD (ChanA+CHP_SkpCnt),HL
	LD (ChanB+CHP_SkpCnt),HL
	LD (ChanC+CHP_SkpCnt),HL
	LD A,L
	LD (DelyCnt),A

	LD HL,EMPTYORN
	LD (ChanA+CHP_OrnPtr),HL
	LD (ChanB+CHP_OrnPtr),HL
	LD (ChanC+CHP_OrnPtr),HL

;note table creator (same as PT3 table $1)
;(c)Ivan Roshin

	LD HL,NTBL

	LD DE,NT_

	LD B,12
L1:	PUSH BC
	LD C,(HL)
	INC HL
	PUSH HL
	LD B,(HL)

	PUSH DE
	EX DE,HL
	LD DE,23
	.db $DD,$26,$08

L2:	SRL B
	RR C
	LD (HL),C
	INC HL
	LD (HL),B
	ADD HL,DE
	.db $DD,$25
	JR NZ,L2

	POP DE
	INC DE
	INC DE
	POP HL
	INC HL
	POP BC
	DJNZ L1

	LD A,$FD
	LD (NT_+$2E),A
	LD A,$0A
	LD (NT_+$5C),A

;vol table creator (same as PT3.5+ table)
;(c)Ivan Roshin

	LD HL,$11
	LD D,H
	LD E,H

	LD IX,VT_+16
	LD C,$10

INITV2:  PUSH HL

	ADD HL,DE
	EX DE,HL
	SBC HL,HL

INITV1:  LD A,L
	RLA
	LD A,H
	ADC A,0
	LD (IX),A
	INC IX
	ADD HL,DE
	INC C
	LD A,C
	AND 15
	JR NZ,INITV1

	POP HL
	LD A,E
	CP $77
	JR NZ,M3
	INC E
M3:      LD A,C
	AND A
	JR NZ,INITV2

	JP ROUT_A0

PhF2PT:
;Convert PT v2.4 Phantom Family to standard PT2
;No integrity checking (can deadlock)

	PUSH HL
	DEC HL
	LD B,32+16
	CALL SUBAREA
	LD C,(HL)
	INC HL
	LD B,(HL)
	PUSH BC
	LD BC,31
	ADD HL,BC
MFLP:	LD A,(HL)
	ADD A,A
	JR C,MFOUND
	INC HL
	RRCA
	CP B
	JR C,MFLP
	LD B,A
	JR MFLP
MFOUND:	INC B
	LD A,B
	ADD A,A
	ADD A,B
	LD HL,(MODADDR)
	POP BC
	ADD HL,BC
	LD B,A
	CALL SUBAREA
	POP HL
	RET

SUBAREA:	LD A,(HL)
	SUB E
	LD (HL),A
	INC HL
	LD A,(HL)
	SBC A,D
	LD (HL),A
	INC HL
	DJNZ SUBAREA
	RET

PD_SAM:	ADD A,A
	LD E,A
	LD D,0
SamPtrs: .equ $+1
	LD HL,$2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
MODADDR: .equ $+1
	LD HL,$2121
	ADD HL,DE
	LD (IX+CHP_SamPtr),L
	LD (IX+CHP_SamPtr+1),H
	JR PD_LOOP

PD_EOff:	LD (IX+CHP_Env_En),A
	JR PD_LOOP

PD_ENV:	LD (IX+CHP_Env_En),16
	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (AYREGS+Env),HL
	JR PD_LOOP

PD_ORN:	ADD A,A
	LD E,A
	LD D,0
OrnPtrs:	.equ $+1
	LD HL,$2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
MDADDR2:	.equ $+1
	LD HL,$2121
	ADD HL,DE
	LD (IX+CHP_OrnPtr),L
	LD (IX+CHP_OrnPtr+1),H
	JR PD_LOOP

PD_SKIP:	INC A
	LD (IX+CHP_Skip),A
	JR PD_LOOP

PD_VOL:	LD (IX+CHP_Volume),A
	JR PD_LOOP

PD_DEL:	LD A,(BC)
	INC BC
	LD (Delay),A
	JR PD_LOOP

PD_GLIS:	SET 2,(IX+CHP_Flags)
	SET 1,(IX+CHP_Flags)
	LD A,(BC)
	INC BC
        LD (IX+CHP_TSlStp),A
	ADD A,A
	SBC A,A
	LD (IX+CHP_TSlStp+1),A
	SCF
	JR PD_LP2

PTDECOD: AND A

PD_LP2:	EX AF,AF'

PD_LOOP:	LD A,(BC)
	INC BC
	ADD A,$20
	JR Z,PD_REL
	JR C,PD_SAM
	ADD A,96
	JR C,PD_NOTE
	INC A
	JR Z,PD_EOff
	ADD A,15
	JP Z,PD_QUIT
	JR C,PD_ENV
	ADD A,$10
	JR C,PD_ORN
	ADD A,$40
	JR C,PD_SKIP
	ADD A,$10
	JR C,PD_VOL
	INC A
	JR Z,PD_DEL
	INC A
	JR Z,PD_GLIS
	INC A
	JR Z,PD_PORT
	INC A
	JR Z,PD_STOP
	LD A,(BC)
	INC BC
	LD (IX+CHP_AddToN),A
	JR PD_LOOP

PD_PORT: RES 2,(IX+CHP_Flags)
	SET 1,(IX+CHP_Flags)
	LD A,(BC)
	INC BC
	INC BC ;ignoring precalc delta to right sound
	INC BC
	SCF
	JR PD_LP2

PD_STOP:	RES 1,(IX+CHP_Flags)
	JR PD_LOOP

PD_REL:	LD (IX+CHP_Flags),A
	JR PD_EXIT

PD_NOTE:	LD L,A
	SET 0,(IX+CHP_Flags)
	EX AF,AF'
	JR NC,NOGLISS
	BIT 2,(IX+CHP_Flags)
	JR NZ,NOPORT
	LD (IX+CHP_SlToNt),L

	PUSH BC
	LD DE,NT_
	SLA L
	LD H,0
	ADD HL,DE
	LD C,(HL)
	INC HL
	LD B,(HL)
	LD L,(IX+CHP_Note)
	SLA L
	LD H,0
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	LD L,C
	LD H,B
	SBC HL,DE
	LD (IX+CHP_TnDelt),L
	LD (IX+CHP_TnDelt+1),H
	JP P,DELTP
	AND A
	JP M,SET_STP
	JR NEG_STP
DELTP:	AND A
	JP P,SET_STP
NEG_STP:	NEG
SET_STP:	LD (IX+CHP_TSlStp),A
	ADD A,A
	SBC A,A
	LD (IX+CHP_TSlStp+1),A
	POP BC
	JR PD_EX1

NOGLISS:	RES 1,(IX+CHP_Flags)
NOPORT:	LD (IX+CHP_Note),L


PD_EX1:	XOR A

PD_EXIT:	LD (IX+CHP_PsInSm),A
	LD (IX+CHP_PsInOr),A
	LD (IX+CHP_CrTnSl),A
	LD (IX+CHP_CrTnSl+1),A
PD_QUIT:	LD A,(IX+CHP_Skip)
	LD (IX+CHP_SkpCnt),A
	RET

CHREGS:	XOR A
	LD (Ampl),A
	PUSH HL
	BIT 0,(IX+CHP_Flags)
	JP Z,CH_EXIT
	LD (CSP_+1),SP
	LD L,(IX+CHP_OrnPtr)
	LD H,(IX+CHP_OrnPtr+1)
	LD SP,HL
	POP DE
	LD H,A
	LD A,(IX+CHP_PsInOr)
	LD L,A
	ADD HL,SP
	INC A
	CP E
	JR C,CH_ORPS
	LD A,D
CH_ORPS:	LD (IX+CHP_PsInOr),A
	LD A,(IX+CHP_Note)
	ADD A,(HL)
	JP P,CH_NTP
	XOR A
CH_NTP:	CP 96
	JR C,CH_NOK
	LD A,95
CH_NOK:	ADD A,A
	EX AF,AF'
	LD L,(IX+CHP_SamPtr)
	LD H,(IX+CHP_SamPtr+1)
	LD SP,HL
	POP DE
	LD H,0
	LD A,(IX+CHP_PsInSm)
	LD B,A
	ADD A,A
	ADD A,B
	LD L,A
	ADD HL,SP
	LD SP,HL
	LD A,B
	INC A
	CP E
	JR C,CH_SMPS
	LD A,D
CH_SMPS:	LD (IX+CHP_PsInSm),A
	POP BC
	POP DE
	LD D,B
	LD L,(IX+CHP_CrTnSl)
	LD H,(IX+CHP_CrTnSl+1)
	BIT 2,C
	JR Z,TSUB
	ADD HL,DE
	ADD HL,DE
TSUB:	EX AF,AF'
	SBC HL,DE
	EX DE,HL
	LD L,A
	LD H,0
	LD SP,NT_
	ADD HL,SP
	LD SP,HL
	POP HL
	ADD HL,DE
CSP_:	LD SP,$3131
	EX (SP),HL

	BIT 1,(IX+CHP_Flags)
	JR Z,CH_AMP
	LD L,(IX+CHP_CrTnSl)
	LD H,(IX+CHP_CrTnSl+1)
	LD E,(IX+CHP_TSlStp)
	LD D,(IX+CHP_TSlStp+1)
	ADD HL,DE
	LD (IX+CHP_CrTnSl),L
	LD (IX+CHP_CrTnSl+1),H
	BIT 2,(IX+CHP_Flags)
	JR NZ,CH_AMP
	LD E,(IX+CHP_TnDelt)
	LD D,(IX+CHP_TnDelt+1)
	LD A,(IX+CHP_TSlStp+1)
	AND A
	JR Z,CH_STPP
	EX DE,HL
CH_STPP: SBC HL,DE
	JP M,CH_AMP
	LD A,(IX+CHP_SlToNt)
	LD (IX+CHP_Note),A
	XOR A
	RES 1,(IX+CHP_Flags)
	LD (IX+CHP_CrTnSl),A
	LD (IX+CHP_CrTnSl+1),A

CH_AMP:	LD A,B
	AND $F0
CH_VOL:	OR (IX+CHP_Volume)
	RRCA
	RRCA
	RRCA
	RRCA
	LD L,A
	LD H,0
	LD DE,VT_
	ADD HL,DE
	LD A,(HL)
	OR (IX+CHP_Env_En)
	LD (Ampl),A
	RRC C
	SBC A,A
	AND $40
	JR NZ,NONS
	LD A,C
	RRCA
	RRCA
	ADD A,(IX+CHP_AddToN)
	LD (AYREGS+Noise),A
	XOR A
NONS:	RRC C
	JR NC,CH_EXIT
	OR 8

CH_EXIT:	LD HL,AYREGS+Mixer
	OR (HL)
	RRCA
	LD (HL),A
	POP HL
	RET

PLAY:    XOR A
	LD (AYREGS+Mixer),A
	DEC A
	LD (AYREGS+EnvTp),A
	LD HL,DelyCnt
	DEC (HL)
	JR NZ,PL2
	LD HL,ChanA+CHP_SkpCnt
	DEC (HL)
	JR NZ,PL1B
AdInPtA:	.equ $+1
	LD BC,$0101
	LD A,(BC)
	AND A
	JR NZ,PL1A
	LD D,A
	LD HL,(CrPsPtr)
	INC HL
	LD A,(HL)
	ADD A,A
	JR NC,PLNLP
	CALL CHECKLP
LPosPtr:	.equ $+1
	LD HL,$2121
	LD A,(HL)
	ADD A,A
PLNLP:	LD (CrPsPtr),HL
	ADD A,(HL)
	ADD A,A
	LD E,A
	RL D
PatsPtr:	.equ $+1
	LD HL,$2121
	ADD HL,DE
	LD DE,(MODADDR)
	LD (PSP_+1),SP
	LD SP,HL
	POP HL
	ADD HL,DE
	LD B,H
	LD C,L
	POP HL
	ADD HL,DE
	LD (AdInPtB),HL
	POP HL
	ADD HL,DE
	LD (AdInPtC),HL
PSP_:	LD SP,$3131
PL1A:	LD IX,ChanA
	CALL PTDECOD
	LD (AdInPtA),BC

PL1B:	LD HL,ChanB+CHP_SkpCnt
	DEC (HL)
	JR NZ,PL1C
	LD IX,ChanB
AdInPtB:	.equ $+1
	LD BC,$0101
	CALL PTDECOD
	LD (AdInPtB),BC

PL1C:	LD HL,ChanC+CHP_SkpCnt
	DEC (HL)
	JR NZ,PL1D
	LD IX,ChanC
AdInPtC:	.equ $+1
	LD BC,$0101
	CALL PTDECOD
	LD (AdInPtC),BC

Delay:	.equ $+1
PL1D:	LD A,$3E
	LD (DelyCnt),A

PL2:	LD IX,ChanA
	LD HL,(AYREGS+TonA)
	CALL CHREGS
	LD (AYREGS+TonA),HL
	LD A,(Ampl)
	LD (AYREGS+AmplA),A
	LD IX,ChanB
	LD HL,(AYREGS+TonB)
	CALL CHREGS
	LD (AYREGS+TonB),HL
	LD A,(Ampl)
	LD (AYREGS+AmplB),A
	LD IX,ChanC
	LD HL,(AYREGS+TonC)
	CALL CHREGS
	LD (AYREGS+TonC),HL

;MSX version of ROUT (c)Dioniso
ROUT:	XOR A
ROUT_A0:
	LD C,$A0
	LD HL,AYREGS
LOUT:	OUT (C),A
	INC C
	OUTI 
	DEC C
	INC A
	CP 8
	JR NZ,LOUT
	OUT ($A0),A
	LD A,(SETUP)
	AND %00000010
	JR Z,R08_
	XOR A
	OUT ($A1),A
	JR R09
R08_
	LD A,(HL)
	OUT ($A1),A
R09:
	LD A,9
	INC HL
	OUT ($A0),A
	LD A,(SETUP)
	AND %00000100
	JR Z,R09_
	XOR A
	OUT ($A1),A
	JR R10
R09_:
	LD A,(HL)
	OUT ($A1),A
R10:
	LD A,10
	INC HL
	OUT ($A0),A
	LD A,(SETUP)
	AND %00001000
	JR Z,R10_
	XOR A
	OUT ($A1),A
	JR R11
R10_:
	LD A,(HL)
	OUT ($A1),A
R11:
	LD A,11
	INC HL
	OUT ($A0),A
	LD A,(HL)
	OUT ($A1),A
	LD A,12
	INC HL
	OUT ($A0),A
	LD A,(HL)
	OUT ($A1),A
	LD A,13
	INC HL
	OUT ($A0),A
	LD A,(HL)
	AND A
	RET M
	OUT ($A1),A
	RET

NTBL:	.dw $0EF8*2,$0E10*2,$0D60*2,$0C80*2,$0BD8*2,$0B28*2
	.dw $0A88*2,$09F0*2,$0960*2,$08E0*2,$0858*2,$07E0*2

;vars from here can be stripped
;you can move VARS to any other address

VARS:

DelyCnt:	.db 0

CHP_PsInSm:	.equ 0
CHP_PsInOr:	.equ 1
CHP_CrTnSl: .equ 2

CHP_Flags:	.equ 4
CHP_TSlStp:	.equ 5
CHP_SamPtr:	.equ 7
CHP_OrnPtr:	.equ 9
CHP_SlToNt:	.equ 11
CHP_Note:	.equ 12
CHP_TnDelt: .equ 13
CHP_Skip:	.equ 15
CHP_SkpCnt:	.equ 16
CHP_Volume:	.equ 17
CHP_Env_En:	.equ 18
CHP_AddToN:	.equ 19
CHP_Size:	.equ 20

ChanA:	.ds CHP_Size
ChanB:	.ds CHP_Size
ChanC:	.ds CHP_Size

AYREGS:	.ds 14

Ampl:	.equ AYREGS+AmplC

VT_:	.ds 256

VAR0END:	.equ VT_+16 ;zeroing area end

EMPTYORN: .equ VT_+31 ;1,0,0 sequence

NT_:	.ds 192

VARSEND:

MDLADDR:	.equ $

;Release 0 steps
;7.Feb.2005
;note table creator; volume table creator; pattern decoder
;8.Feb.2005
;CHREGS subroutine
;9.Feb.2005
;no code
;10.Feb.2005
;PLAY subroutine
;11.Feb.2005
;INIT subroutine; first run; debug
;12.Feb.2005
;debug; test; first release!

;Tests in IMMATION TESTER V1.0 by Andy Man/POS
;Module name/author	Min tacts	Max tacts	Average
;Epilogue/Nik-O		3000		8280		5000
;NY tHEMEs/zHenYa		3000		7704		5000
;GUEST 4/Alex Job		2232		7720		5000
;KickDB/Fatal Snipe	1464		8456		5000

;Size:
;Code block $43D bytes
;Variables $20B bytes (can be stripped)
;Size in RAM $43D+$20B=$648 (1608) bytes
