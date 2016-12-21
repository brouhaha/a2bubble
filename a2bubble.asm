; Helix Apple II bubble memory card firmware version 3.40.

; Partially reverse-engineered by Eric Smith <spacewar@gmail.com>

; Cross-assemble with Macro Assembler AS:
;   http://john.ccac.rwth-aachen.de:8000/as/

fillto	macro	endaddr,value,{noexpand}
	ifnb	value
v	set	value
	else
v	set	$00
	endif
	while	*<endaddr
	if	(endaddr-*)>1024
	fcb	[1024] v
	else
	fcb	[endaddr-*] v
	endif
	endm
	endm

fcstrm	macro	s
	irpc	c,s
	fcb	'c'|$80
	endm
	endm


; ProDOS disk drivers are allowed to use zero page locations
; 00-01, 3a-46

Z18	equ	$18
Z26	equ	$26
Z27	equ	$27
Z2a	equ	$2a

Z2b	equ	$2b	; DOS: slot * 16

Z2c	equ	$2c	; DOS: pointer to BMC data register (two bytes)

Z2e	equ	$2e
Z2f	equ	$2f
Z35	equ	$35

cswl	equ	$36	; monitor: character output vector (two bytes)

Z38	equ	$38
Z3a	equ	$3a
Z3b	equ	$3b

Z3c	equ	$3c	; ProDOS: pointer to BMC data register (two bytes)
Z3d	equ	$3d

Z3e	equ	$3e	; ProDOS: slot * 16

Z3f	equ	$3f
Z41	equ	$41

prodos_cmd	equ	$42
prodos_unit	equ	$43
prodos_buf	equ	$44
prodos_block	equ	$46	; two bytes

dos_retry_count	equ	$47

Z48	equ	$48
Zbd	equ	$bd
Zbe	equ	$be
Zbf	equ	$bf
Zc3	equ	$c3
Zc5	equ	$c5
Zcb	equ	$cb

stack	equ	$0100

D03a1	equ	$03a1
D03a2	equ	$03a2
D03a4	equ	$03a4
D03a8	equ	$03a8
D03a9	equ	$03a9
D03ac	equ	$03ac
D03ad	equ	$03ad
D03af	equ	$03af

; Apple DOS: locate file manager input parameter list
S03dc	equ	$03dc
D03e0	equ	$03e0
D03e1	equ	$03e1
D03e2	equ	$03e2

; Apple DOS: locate input parameter list for RWTS
D03e3	equ	$03e3
D03e4	equ	$03e4
D03e5	equ	$03e5
D03e6	equ	$03e6
D03e7	equ	$03e7
D03e8	equ	$03e8
D03e9	equ	$03e9

; Apple DOS: JMP to subroutine to reconnect DOS keyboard and screen intercepts
D03ea	equ	$03ea
D03eb	equ	$03eb

D05f8	equ	$05f8

D0800	equ	$0800
L0801	equ	$0801

Sbfd0	equ	$bfd0	; ProDOS irqxit?


; Apple II hardware
spkr			equ	$c030
c8xx_rom_disable	equ	$cfff

; Applesoft entry point
Sd382	equ	$d382


; Apple II BASIC entry points (Integer or Applesoft)
basic_cold_start	equ	$e000

; Apple II monitor ROM entry points
Dfb59	equ	$fb59
cout	equ	$fded
cout1	equ	$fdf0	; output char to screen
monrts	equ	$ff58	; guaranteed to be an RTS instruction


; card-specific hardware
; Intel 7220-1 Bubble Memory Controller hardware registers
bmc_data   	equ	$c080	; but data reg only addressed indirectly
bmc_cmd		equ	$c081
bmc_status	equ	$c081

; unknown card hardware
Dc088	equ	$c088
Dc089	equ	$c089
Dc08c	equ	$c08c
Dc08e	equ	$c08e


; BMC internal register addresses
; These registers are accessed by writing the register address to the
; bmc_cmd register, then reading/writing the bmc_data register
bmc_reg_fifo		equ	$00	; RW, doesn't advance
bmc_reg_utility		equ	$0a	; RW
bmc_reg_block_length	equ	$0b	; two bytes, little-endian, WO
bmc_reg_enable		equ	$0d	; WO
bmc_reg_address		equ	$0e	; two bytes, RW

; BMC commands
bmc_cmd_initialize	equ	$11
bmc_cmd_read_data	equ	$12
bmc_cmd_write_data	equ	$13
bmc_cmd_abort		equ	$19
bmc_cmd_reset_fifo	equ	$1d


	org	$cf00
	phase	$c600

; Bytes at Cn0{1,3,5,7} = $20, $00, $03, $3C identify card as bootable by
;   original Autostart ROM monitor. Later models only check the first
;   three of those. Result is that SmartPort are not automatically bootable
;   with original Autostart monitor.
; Bytes at Cn0{1,3,5} = $20, $00, $03 identify card as a block device
; Byte at Cn07 = $3C for Disk II,
;                $00 for SmartPort interface supported
; Byte at CnFF = $00 for 16-sector Disk II, $ff for 13-sector Disk II,
;   other value $xx = ProDOS block device, entry point = Cnxx, in this case
;   $Cn2A
; For ProDOS block device:
;   word at CnFC = device size in blocks,
;   byte at CnFE = STATUS byte

	ldx	$20	; $Cn01 = $20
	ldy	#$00	; $Cn03 = $00
	ldx	#$03	; $Cn05 = $03
	stx	Z3c	; $Cn07 = $3C, bootable by original Autostart ROM,
			;              no SmartPort support

; get the slot number we're running in
	jsr	monrts
	tsx
	lda	stack,X
	sta	Z3f		; $Cn for slot n
	
	asl
	asl
	asl
	asl
	sta	Z2b		; $n0, slot number * 16

	tax
	lda	Dc089,X
	lda	#$00
	sta	Z26
	sta	Z3d
	sta	Z41
	lda	#$08
	sta	Z27
	clc
	bcc	boot		; always taken

prodos_entry_x:
	lda	c8xx_rom_disable		; disable any other C800 ROMs
	jmp	prodos_entry_xx


	fcstrm	"COPR. HELIX LABORATORIES INC. 1984"


	fillto	$c65c,$ff
	
; DOS 3.3 boot sector reenters ROM bootstrap here
boot:	lda	c8xx_rom_disable
	jsr	click
	ldx	Z2b
	lda	#$00
	sta	Z2f
	lda	Z41
	sec
	sbc	#$03
	bpl	Lc671
	lda	Z41
Lc671:	sta	Z41
	clc
	asl
	asl
	asl
	asl
	rol	Z2f
	sta	Z2e
	ldy	Z41
	bne	Lc687
	ldy	Z3d
	lda	Dcae8,Y
	bpl	Lc697
Lc687:	ldy	Z3d
	lda	Dcad8,Y
	ldy	Z41
	cpy	#$0e
	bne	Lc697
	clc
	adc	#$01
	and	#$0f
Lc697:	clc
	adc	Z2e
	asl
	rol	Z2f
	asl
	rol	Z2f
	sta	Z2e

	lda	c8xx_rom_disable
	jsr	initchk
	bcc	Lc6b3

	lda	#$04
	sta	dos_retry_count

Lc6ae:	jsr	initial
	bcs	Lc6cb

Lc6b3:	jsr	rdbub
	bcs	Lc6cb

	lda	spkr		; click speaker
	inc	Z27
	inc	Z3d
	lda	Z3d
	jsr	Sce83
	ldx	Z2b
	bcc	boot
	jmp	L0801		; proceed to next stage of boot sequence

Lc6cb:	dec	dos_retry_count
	bne	Lc6ae

	lda	Dc088,X

; output IO error message to screen

	lda	#(cout1 & $ff)	; disconnect any alternate character output
	sta	cswl
	lda	#(cout1 >> 8)
	sta	cswl+1

	lda	#msg_io_error & $ff
	sta	Z3e

	ldy	#$00
msg_loop:
	lda	(Z3e),Y
	jsr	cout
	iny
	cpy	#msg_len_io_error
	bne	msg_loop
	jmp	basic_cold_start

msg_io_error:	fcb	$87,$8d,$8a	; bell, return, linefeed
	fcb	$be,$c9,$cf,$a0,$c5,$d2,$d2,$cf,$d2	; >IO ERROR
msg_len_io_error	equ	*-msg_io_error

	fillto	$c6fb,$ff

; CnFB would be SmartPort ID Type Byte, SmartPort Interface was supported,
; but it's not.
	fcb	$ff

; must be at CnFC here!
	fdb	$0100	; total number of 512-byte blocks on device
	fcb	$4f	; ProDOS block device STATUS byte
			; not removable
			; interruptable
			; 1 volume
			; supports formatting, write, read, status
	fcb	prodos_entry_x & $ff

	dephase


	org	$c800

; Jump table, first 12 entries described in Appendix E of manual
	jmp	bubrwts
	jmp	initchk
	jmp	cmdwait
	jmp	ldrdrac
	jmp	ldwrrac
	jmp	initial
	jmp	wptest
	jmp	rdbub
	jmp	wrbub
	jmp	inbitmap
	jmp	click
	jmp	bubpasc

	jmp	Lcbd2

prodos_entry_xx:
	jmp	prodos_entry


; reserved room for a few additional jumps for future expansion
	fillto	$c830, $ff


; Bubble RWTS, called by the patch to DOS 3.3
bubrwts:
	pla
	sta	Z3c
	pla
	sta	Z3d
	lda	#$04
	pha
	stx	Z2b
	stx	D05f8
	txa
	ldy	#$0f
	cmp	(Z48),Y
	beq	Lc860
	txa
	pha
	lda	(Z48),Y
	tax
	pla
	pha
	sta	(Z48),Y
	lda	Dc08e,X
Lc851:	ldy	#$08
	lda	Dc08c,X
Lc856:	cmp	Dc08c,X
	bne	Lc851
	dey
	bne	Lc856
	pla
	tax
Lc860:	lda	Dc089,X
	jsr	click
	ldy	#$08
	lda	(Z48),Y
	sta	Z26
	iny
	lda	(Z48),Y
	sta	Z27
	ldy	#$0e
	lda	#$fe
	sta	(Z48),Y
	ldy	#$02
	lda	(Z48),Y
	ldy	#$10
	sta	(Z48),Y
	ror
	ror
	sta	Z35
	ldy	#$04
	lda	(Z48),Y
	sta	Z2a
	sec
	sbc	#$03
	bpl	Lc890
	lda	Z2a
Lc890:	sta	Z2a
	tay
	sec
	sbc	#$20
	bpl	Lc908
	clc
	lda	#$00
	sta	Z2f
	tya
	asl
	asl
	asl
	asl
	rol	Z2f
	sta	Z2e
	ldy	#$05
	lda	(Z48),Y
	tay
	lda	Dcac8,Y
	ldy	Z2a
	bne	Lc8b8
	tay
	lda	Dcae8,Y
	bpl	Lc8c7
Lc8b8:	tay
	lda	Dcad8,Y
	ldy	Z2a
	cpy	#$0e
	bne	Lc8c7
	clc
	adc	#$01
	and	#$0f
Lc8c7:	clc
	adc	Z2e
	asl
	rol	Z2f
	asl
	rol	Z2f
	sta	Z2e
	jsr	initchk
	bcc	Lc8dc
Lc8d7:	jsr	initial
	bcs	Lc8fd
Lc8dc:	ldy	#$0c
	lda	(Z48),Y
	beq	Lc908
	lsr
	bcc	Lc8eb
	jsr	rdbub
	jmp	Lc8fd

Lc8eb:	lsr
	bcc	Lc908
	jsr	wptest
	bcc	Lc8f7
	lda	#$10
	bne	Lc920
Lc8f7:	jsr	inbitmap
	jsr	wrbub
Lc8fd:	bcc	Lc908
	pla
	sec
	sbc	#$01
	pha
	bne	Lc8d7
	beq	Lc91a
Lc908:	ldy	#$06
	lda	(Z48),Y
	sta	Z3c
	iny
	lda	(Z48),Y
	sta	Z3d
	jsr	click
	lda	#$00
	beq	Lc91e
Lc91a:	lda	#$40
	bne	Lc920
Lc91e:	clc
	fcb	$24	; bit instr to skip sec
Lc920:	sec
	ldy	#$0d
	sta	(Z48),Y
	lda	Dc088,X
	pla
	rts


; check whether 7220 BMC needs initializing
initchk:
	lda	#bmc_cmd_reset_fifo
	sta	bmc_cmd,X
	jsr	cmdwait
	rts


; wait for BMC to go busy, then idle
cmdwait:
	lda	#$00
	pha
Lc936:	lda	bmc_status,X
	bmi	Lc940
	dey
	beq	Lc94f
	bne	Lc936

Lc940:	lda	bmc_status,X
	bpl	Lc951
	dey
	bne	Lc940
	pla
	sec
	sbc	#$01
	pha
	bne	Lc940

Lc94f:	sec
	fcb	$24	; bit instr to skip clc
Lc951:	clc
	pla
	rts


; send block length and address to BMC for read data command
; with error correction
; used for DOS
; (enables MFBTR, can't be used for commands other than read data)
; on entry, Y = number of 64-byte pages for block length
ldrdrac:
	lda	#bmc_reg_block_length
	sta	bmc_cmd,X
	ldx	#$00

	tya
	sta	(Z2c,X)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z2c,X)		; write to block length MSB

	lda	#$28		; enable RCD (read corrected data)
				;   and MFBTR (max FSA-BMC transfer rate)
	sta	(Z2c,X)		; write to enable register

	lda	Z2e
	sta	(Z2c,X)		; write to address register LSB

	lda	Z2f
	sta	(Z2c,X)		; write to address register MSB

	ldx	Z2b
	rts


; send block length and address to BMC for commands other than read data,
; including write with error correction
; used for DOS
; (disables MFBTR)
; on entry, Y = number of 64-byte pages for block length
ldwrrac:
	lda	#bmc_reg_block_length
	sta	bmc_cmd,X
	ldx	#$00

	tya
	sta	(Z2c,X)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z2c,X)		; write to block length MSB

	lda	#$20		; enable RCD (read corrected data) only
	sta	(Z2c,X)		; write to enable register

	lda	Z2e
	sta	(Z2c,X)		; write to address register LSB

	lda	Z2f
	sta	(Z2c,X)		; write to address register MSB

	ldx	Z2b
	rts


; DOS: set Z2c to point to data reg
dos_setup_data_reg_ptr:
	lda	Z2b
	clc
	adc	#$80
	sta	Z2c
	lda	#$c0
	sta	Z2c+1
	rts


; initialize: tell BMC to abort, twice
initial:
	lda	#bmc_cmd_abort
	sta	bmc_cmd,X
	jsr	cmdwait
	bcs	Lc9dc

	lda	#bmc_cmd_abort
	sta	bmc_cmd,X
	jsr	cmdwait
	bcs	Lc9dc

	lda	bmc_status,X
	cmp	#$40
	bne	Lc9dc

; delay
	sec
	ldy	#$64
Lc9b8:	lda	#$ff
Lc9ba:	sbc	#$01
	bne	Lc9ba
	dey
	bne	Lc9b8

	jsr	dos_setup_data_reg_ptr
	ldy	#$04	; 4*64 = 256 byte block
	jsr	ldwrrac

	lda	#bmc_cmd_initialize
	sta	bmc_cmd,X
	jsr	cmdwait
	bcs	Lc9dc

	lda	bmc_status,X
	cmp	#$40
	bne	Lc9dc
	clc
	fcb	$24	; bit instr to skip sec
Lc9dc:	sec
	rts


Sc9de:	lda	#$ff
	pha
Lc9e1:	lda	bmc_status,X
	bmi	Lc9ef
	pla
	sec
	sbc	#$01
	pha
	bne	Lc9e1
	sec
	fcb	$24	; bit instr to skip clc
Lc9ef:	clc
	pla
	rts

Sc9f2:	lda	#$ff
	pha
	pha
Lc9f6:	lda	bmc_status,X
	bpl	Lca13
	lsr
	bcs	Lca15
	pla
	sec
	sbc	#$01
	pha
	bne	Lc9f6
	pla
	pla
	sec
	sbc	#$01
	pha
	beq	Lca12
	lda	#$ff
	pha
	bne	Lc9f6
Lca12:	pha
Lca13:	sec
	fcb	$24	; bit instr to skip clc
Lca15:	clc
	pla
	pla
	rts


; test write-protect switch
; returns carry set if write-protected, clear if not
wptest:
	lda	Dc08c,X
	bpl	Lca20
	clc
	fcb	$24	; bit instr to skip sec
Lca20:	sec
	rts


; Read one sector (256 bytes), used for DOS
rdbub:
	jsr	dos_setup_data_reg_ptr
	ldy	#$04	; 4*64 = 256 byte block
	jsr	ldrdrac
	ldy	#$00

	lda	#bmc_cmd_read_data
	sta	bmc_cmd,X
	jsr	Sc9de
	bcs	Lca52

; read data loop
Lca36:	jsr	Sc9f2
	bcs	Lca47
	ldx	#$00
	lda	(Z2c,X)
	ldx	Z2b
	sta	(Z26),Y
	iny
	jmp	Lca36

Lca47:	beq	Lca52
	lda	bmc_status,X
	and	#$3c
	bne	Lca52
	clc
	fcb	$24	; bit instr to skip sec
Lca52:	sec
	rts


; Write one sector (256 bytes), used for DOS
wrbub:	jsr	dos_setup_data_reg_ptr
	ldy	#$04	; 4*64 = 256 byte block
	jsr	ldwrrac
	ldy	#$00

	lda	#bmc_cmd_write_data
	sta	bmc_cmd,X
	jsr	Sc9de
	bcs	Lca84

; write data loop
Lca68:	jsr	Sc9f2
	bcs	Lca79
	lda	(Z26),Y
	iny
	ldx	#$00
	sta	(Z2c,X)
	ldx	Z2b
	jmp	Lca68

Lca79:	beq	Lca84
	lda	bmc_status,X
	and	#$3c
	bne	Lca84
	clc
	fcb	$24	; bit instr to skip clc
Lca84:	sec
	rts


; Change the DOS VTOC bitmap to protect tracks 3-5 to prevent
; DOS from allocating those to files. Necessary when initializing
; bubble as a bootable DOS disk, because tracks 0-2 (used for DOS)
; and tracks 3-5 (used for files) map to the same portion of the
; bubble device.
inbitmap:
	lda	Z2a
	cmp	#$0e
	bne	Lcabd
	ldy	#$05
	lda	(Z48),Y
	cmp	#$00
	bne	Lcabd
	jsr	S03dc
	sty	Z2c
	sta	Z2c+1
	ldy	#$00
	lda	(Z2c),Y
	cmp	#$0b
	bne	Lcabd
	lda	Z35
	bpl	Lcabd
	lda	#$fe
	ldy	#$06
	sta	(Z26),Y
	lda	#$00
	ldy	#$44
Lcab1:	sta	(Z26),Y
	iny
	cpy	#$4c
	bne	Lcab1
	iny
	lda	#$e0
	sta	(Z26),Y
Lcabd:	rts


; toggle speaker if enabled by switch
click:	lda	Dc08c,X
	asl
	bpl	Lcac7
	lda	spkr
Lcac7:	rts


; interleave tables?
Dcac8:	fcb	$00,$0d,$0b,$09,$07,$05,$03,$01
	fcb	$0e,$0c,$0a,$08,$06,$04,$02,$0f

Dcad8:	fcb	$0f,$08,$01,$09,$02,$0a,$03,$0b
	fcb	$04,$0c,$05,$0d,$06,$0e,$07,$00

Dcae8:	fcb	$00,$08,$01,$09,$02,$0a,$03,$0b
	fcb	$04,$0c,$05,$0d,$06,$0e,$07,$0f


; disk access from Apple Pascal 1.1 patch
bubpasc:
	lda	#$04
	pha
	ldx	D03a1
	stx	Z2b
	cpx	D03af
	beq	Lcb16
	ldx	D03af
Lcb08:	jsr	Sbfd0
	jsr	Sd382
	bne	Lcb08
	ldx	D03a1
	stx	D03af
Lcb16:	lda	D03a2
	lsr
	bcs	Lcb21
	lda	#$09
	jmp	Lcbc9

Lcb21:	lda	Dc089,X
	lda	D03a4
	tay
	sec
	sbc	#$20
	bmi	Lcb32
	lda	#$08
	jmp	Lcbc9

Lcb32:	clc
	lda	#$00
	sta	Z2f
	tya
	asl
	asl
	asl
	asl
	rol	Z2f
	sta	Z2e
	lda	Zbd
	sta	Zcb
	clc
	adc	Z2e
	asl
	rol	Z2f
	asl
	rol	Z2f
	sta	Z2e
Lcb4f:	jsr	Sbfd0
	jsr	click
	lda	Zcb
	sec
	sbc	Zbd
	tay
	clc
	adc	D03a9
	iny
	cpy	Zbf
	bcc	Lcb77
	beq	Lcb6b
	lda	#$40
	jmp	Lcbc9

Lcb6b:	ldy	Zbe
	beq	Lcb77
	sta	Zc5
	ldy	#$00
	lda	#$02
	bcs	Lcb7a
Lcb77:	ldy	D03a8
Lcb7a:	sty	Z26
	sta	Z27
	sty	Z3e
	sta	Z3f
	jsr	initchk
	bcc	Lcb8c
Lcb87:	jsr	initial
	bcs	Lcba6
Lcb8c:	lda	D03ac
	cmp	#$02
	bne	Lcba3
	jsr	wptest
	bcc	Lcb9d
	lda	#$10
	jmp	Lcbc9

Lcb9d:	jsr	wrbub
	jmp	Lcba6

Lcba3:	jsr	rdbub
Lcba6:	bcc	Lcbb4
	pla
	sec
	sbc	#$01
	pha
	bne	Lcb87
	lda	#$40
	jmp	Lcbc9

Lcbb4:	inc	Zcb
	lda	#$04
	clc
	adc	Z2e
	sta	Z2e
	lda	#$00
	adc	Z2f
	sta	Z2f
	dec	Zc3
	bne	Lcb4f
	clc
	fcb	$24	; bit instr to skip sec
Lcbc9:	sec
	sta	D03ad
	lda	Dc088,X
	pla
	rts

Lcbd2:	lda	#$04
	pha
	lda	D03e6
	tax
	cmp	D03e7
	beq	Lcbfb
	txa
	tay
	lda	D03e7
	tax
	tya
	pha
	sta	D03e7
	lda	Dc08e,X
Lcbec:	ldy	#$08
	lda	Dc08c,X
Lcbf1:	cmp	Dc08c,X
	bne	Lcbec
	dey
	bne	Lcbf1
	pla
	tax
Lcbfb:	lda	Dc089,X
	jsr	click
	stx	Z2b
	lda	D03e4
	sta	D03e5
	cmp	#$01
	beq	Lcc12
	lda	#$40
	jmp	Lcca0

Lcc12:	lda	D03e8
	sta	Z26
	lda	D03e9
	sta	Z27
	lda	D03e2
	sta	D03e3
	lda	D03e0
	tay
	sec
	sbc	#$20
	bpl	Lcc93
	clc
	lda	#$00
	sta	Z2f
	tya
	asl
	asl
	asl
	asl
	rol	Z2f
	sta	Z2e
	lda	D03e1
	tay
	lda	Dcca9,Y
	ldy	D03e0
	bne	Lcc4b
	tay
	lda	Dcae8,Y
	bpl	Lcc4f
Lcc4b:	tay
	lda	Dcad8,Y
Lcc4f:	clc
	adc	Z2e
	asl
	rol	Z2f
	asl
	rol	Z2f
	sta	Z2e
	jsr	initchk
	bcc	Lcc64
Lcc5f:	jsr	initial
	bcs	Lcc88
Lcc64:	lda	D03eb
	beq	Lcc7c
	cmp	#$01
	beq	Lcc76
	cmp	#$02
	beq	Lcc7c
	lda	#$40
	jmp	Lcca0

Lcc76:	jsr	rdbub
	jmp	Lcc88

Lcc7c:	jsr	wptest
	bcc	Lcc85
	lda	#$10
	bne	Lcca0
Lcc85:	jsr	wrbub
Lcc88:	bcc	Lcc93
	pla
	sec
	sbc	#$01
	pha
	bne	Lcc5f
	beq	Lcc9a
Lcc93:	jsr	click
	lda	#$00
	beq	Lcc9e
Lcc9a:	lda	#$40	; 64 @
	bne	Lcca0
Lcc9e:	clc
	fcb	$24	; bit instr to skip sec
Lcca0:	sec
	sta	D03ea
	lda	Dc088,X
	pla
	rts

; interleave table?
Dcca9:	fcb	$00,$02,$04,$06,$08,$0a,$0c,$0e
	fcb	$01,$03,$05,$07,$09,$0b,$0d,$0f

prodos_entry:
	cld
	lda	prodos_cmd		; is the command format?
	eor	#$03
	beq	Lccf2		;   yes, do nothing and report success

	lda	prodos_block
	pha
	lda	prodos_block+1
	pha

	beq	Lcccf

	lda	#$27
	sta	Z3f

	jmp	Lcce6


Lcccf:	asl	prodos_block
	rol	prodos_block+1
	asl	prodos_block
	rol	prodos_block+1
	asl	prodos_block
	rol	prodos_block+1
	lda	prodos_buf
	sta	Z3a
	lda	prodos_buf+1
	sta	Z3b
	jsr	Sccf4
Lcce6:	pla
	sta	prodos_block+1
	pla
	sta	prodos_block
	lda	Z3f
	beq	Lccf2

	sec
	fcb	$24	; bit instr to skip clc
Lccf2:	clc
	rts

Sccf4:	lda	#$00
	sta	Z3f
	lda	prodos_unit
	and	#$70
	sta	Z3e
	jsr	Scd5e
	lda	prodos_unit
	sta	Dfb59	; XXX why write to ROM?
	lda	#$04
	pha
	ldx	Z3e
	lda	Dc089,X
	jsr	click
	jsr	initchk
	bcc	Lcd1b
Lcd16:	jsr	Scdcd
	bcs	Lcd45

Lcd1b:	lda	prodos_cmd
	beq	Lcd52		; status request

	cmp	#$03
	beq	Lcd52		; format request

	lsr
	bcs	Lcd37		; read request

; write request here
	jsr	wptest
	bcc	Lcd31
	lda	#$2b
	sta	Z3f
	bne	Lcd52
Lcd31:	jsr	Sce49
	jmp	Lcd45

Lcd37:	lsr
	bcs	Lcd40
	jsr	Sce13
	jmp	Lcd45

Lcd40:	sec
	jmp	Lcd45

	fcb	$18

Lcd45:	bcc	Lcd52
	pla
	sec
	sbc	#$01
	pha
	bne	Lcd16
	lda	#$27
	sta	Z3f
Lcd52:	lda	Dc088,X
	jsr	click
	ldx	#$00
	ldy	#$01
	pla
	rts

Scd5e:	eor	Dfb59
	and	#$7f
	beq	Lcd84
	lda	Dfb59
	and	#$0f
	bne	Lcd84
	lda	Dfb59
	and	#$70
	tax
	beq	Lcd84
	lda	Dc08e,X
Lcd77:	ldy	#$08
	lda	Dc08c,X
Lcd7c:	cmp	Dc08c,X
	bne	Lcd77
	dey
	bne	Lcd7c
Lcd84:	rts


; send block length and address to BMC for read data command
; (enables MFBTR, can't be used for commands other than read data)
setup_prodos_read:	lda	#bmc_reg_block_length
	sta	bmc_cmd,X
	ldx	#$00

	lda	#$08		; 8*64 = 512 byte block
	sta	(Z3c,X)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z3c,X)		; write to block length MSB

	lda	#$28		; enable RCD (read corrected data)
				;   and MFBTR (max FSA-BMC transfer rate)
	sta	(Z3c,X)		; write to enable register

	lda	prodos_block
	sta	(Z3c,X)		; write to address register LSB

	lda	prodos_block+1
	sta	(Z3c,X)		; write to address register MSB

	ldx	Z3e
	rts


; send block length and address to BMC for commands other than read data
; (disables MFBTR)
setup_prodos_nonread:	lda	#bmc_reg_block_length
	sta	bmc_cmd,X
	ldx	#$00

	lda	#$08		; 8*64 = 512 byte block
	sta	(Z3c,X)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z3c,X)		; write to block length MSB

	lda	#$20
	sta	(Z3c,X)		; write to enable register

	lda	prodos_block
	sta	(Z3c,X)		; write to address register LSB

	lda	prodos_block+1
	sta	(Z3c,X)		; write to address register MSB

	ldx	Z3e
	rts


; set Z3c to point to data reg
prodos_setup_data_reg_ptr:
	lda	Z3e
	clc
	adc	#$80
	sta	Z3c
	lda	#$c0
	sta	Z3c+1
	rts


Scdcd:	sei

	lda	#bmc_cmd_abort
	sta	bmc_cmd,X
	jsr	cmdwait
	bcs	Lce11

	lda	#bmc_cmd_abort
	sta	bmc_cmd,X
	jsr	cmdwait

	cli

	bcs	Lce11
	lda	bmc_status,X
	cmp	#$40
	bne	Lce11
	sec
	ldy	#$64
Lcded:	lda	#$ff
Lcdef:	sbc	#$01
	bne	Lcdef
	dey
	bne	Lcded
	jsr	prodos_setup_data_reg_ptr
	jsr	setup_prodos_nonread
	sei

	lda	#bmc_cmd_initialize
	sta	bmc_cmd,X
	jsr	cmdwait

	cli
	bcs	Lce11
	lda	bmc_status,X
	cmp	#$40
	bne	Lce11
	clc
	fcb	$24	; bit instr to skip sec
Lce11:	sec
	rts


Sce13:	jsr	prodos_setup_data_reg_ptr
	jsr	setup_prodos_read
	ldy	#$00
	sei

	lda	#bmc_cmd_read_data
	sta	bmc_cmd,X
	jsr	Sc9de
	bcs	Lce46

; read data loop
Lce26:	jsr	Sc9f2
	bcs	Lce3b
	ldx	#$00
	lda	(Z3c,X)
	ldx	Z3e
	sta	(Z3a),Y
	iny
	bne	Lce26
	inc	Z3b
	jmp	Lce26

Lce3b:	beq	Lce46
	lda	bmc_status,X
	and	#$3c
	bne	Lce46
	clc
	fcb	$24	; bit instr to skip sec
Lce46:	sec
	cli
	rts


Sce49:	jsr	prodos_setup_data_reg_ptr
	jsr	setup_prodos_nonread
	ldy	#$00
	sei

	lda	#bmc_cmd_write_data
	sta	bmc_cmd,X
	jsr	Sc9de
	bcs	Lce7c

; write data loop
Lce5c:	jsr	Sc9f2
	bcs	Lce71
	lda	(Z3a),Y
	iny
	bne	Lce68
	inc	Z3b
Lce68:	ldx	#$00
	sta	(Z3c,X)
	ldx	Z3e
	jmp	Lce5c

Lce71:	beq	Lce7c
	lda	bmc_status,X
	and	#$3c
	bne	Lce7c
	clc
	fcb	$24	; bit instr to skip sec
Lce7c:	sec
	cli
	rts


Dce7f:	fcb	$01,$38,$b0,$03            	; ".8.."

Sce83:	cmp	#$01
	bne	Lce99
	ldx	#$03
Lce89:	lda	Dce7f,X
	cmp	D0800,X
	bne	Lce99
	dex
	bpl	Lce89
	inc	Z3d
	clc
	bcc	Lce9e
Lce99:	lda	Z3d
	cmp	D0800
Lce9e:	rts

	fillto	$cef1,$ff

	fcstrm	"11/24/84 VER340"
