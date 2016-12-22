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

bubble_track	equ	$2a

Z2b	equ	$2b

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

Z3e	equ	$3e

Z3f	equ	$3f
Z41	equ	$41


; boot ROM variables
boot_retry_count	equ	$47


; DOS variables
dos_slot_16	equ	$2b	; slot * 16

rwts_dct	equ	$3c
rwts_iopb	equ	$48

; DOS RWTS IOPB offsets
rwts_iopb_table_type	equ	$00
rwts_iopb_slot_num_16	equ	$01	; slot number times 16
rwts_iopb_drive_num	equ	$02
rwts_iopb_volume_num	equ	$03
rwts_iopb_track		equ	$04
rwts_iopb_sector	equ	$05
rwts_iopb_dct_ptr	equ	$06
rwts_iopb_buf_ptr	equ	$08
; byte at offset 0a not used
rwts_iopb_byte_count	equ	$0b
rwts_iopb_command	equ	$0c
rwts_iopb_return_code	equ	$0d
rwts_iopb_prev_volume	equ	$0e
rwts_iopb_prev_slot_16	equ	$0f
rwts_iopb_prev_drive	equ	$10


; prodos variables
prodos_buf_ptr	equ	$3a
prodos_bmc_ptr	equ	$3c	; pointer to BMC data register (two bytes)
prodos_slot_16	equ	$3e	; slot * 16
prodos_cmd	equ	$42
prodos_unit	equ	$43
prodos_buf	equ	$44
prodos_block	equ	$46	; two bytes



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

; Apple DOS
get_dos_fm_parm	equ	$03dc	; locate file manager input parameter list

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
cout	equ	$fded
cout1	equ	$fdf0	; output char to screen
monrts	equ	$ff58	; guaranteed to be an RTS instruction

; ProDOS variable
prodos_prev_unit	equ	$fb59	; in Language Card RAM
	; NOTE: won't work right with Prodos on 48K Apple II or II+


; card-specific hardware
; Intel 7220-1 Bubble Memory Controller hardware registers
bmc_data   	equ	$c080	; but data reg only addressed indirectly
bmc_cmd		equ	$c081
bmc_status	equ	$c081

; unknown card hardware
led_off	equ	$c088
led_on	equ	$c089
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
	lda	stack,x
	sta	Z3f		; $Cn for slot n
	
	asl
	asl
	asl
	asl
	sta	dos_slot_16	; $n0, slot number * 16

	tax
	lda	led_on,x
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
	ldx	dos_slot_16
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
	lda	Dcae8,y
	bpl	Lc697
Lc687:	ldy	Z3d
	lda	Dcad8,y
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
	sta	boot_retry_count

Lc6ae:	jsr	initial
	bcs	Lc6cb

Lc6b3:	jsr	rdbub
	bcs	Lc6cb

	lda	spkr		; click speaker
	inc	Z27
	inc	Z3d
	lda	Z3d
	jsr	Sce83
	ldx	dos_slot_16
	bcc	boot
	jmp	L0801		; proceed to next stage of boot sequence

Lc6cb:	dec	boot_retry_count
	bne	Lc6ae

	lda	led_off,x

; output IO error message to screen

	lda	#(cout1 & $ff)	; disconnect any alternate character output
	sta	cswl
	lda	#(cout1 >> 8)
	sta	cswl+1

	lda	#msg_io_error & $ff
	sta	Z3e

	ldy	#$00
msg_loop:
	lda	(Z3e),y
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

	jmp	Lcbd2		; XXX may be CP/M entry point

prodos_entry_xx:
	jmp	prodos_entry


; reserved room for a few additional jumps for future expansion
	fillto	$c830, $ff


; Bubble RWTS, called by the patch to DOS 3.3
bubrwts:
	pla			; discard return address for DOS patch
	sta	rwts_dct
	pla
	sta	rwts_dct+1

	lda	#$04		; put retry count on stack
	pha

	stx	dos_slot_16
	stx	D05f8
	txa

	ldy	#rwts_iopb_prev_slot_16	; slot matches prev slot?
	cmp	(rwts_iopb),y
	beq	Lc860		; yes, don't have to do anything special

	txa			; turn off prev slot Disk II motor?
	pha
	lda	(rwts_iopb),y
	tax
	pla
	pha
	sta	(rwts_iopb),y
	lda	Dc08e,x
Lc851:	ldy	#$08
	lda	Dc08c,x
Lc856:	cmp	Dc08c,x
	bne	Lc851
	dey
	bne	Lc856
	pla
	tax

Lc860:	lda	led_on,x
	jsr	click

	ldy	#rwts_iopb_buf_ptr	; copy buf ptr from RWTS IOPB to Z26
	lda	(rwts_iopb),y
	sta	Z26
	iny
	lda	(rwts_iopb),y
	sta	Z26+1

	ldy	#rwts_iopb_prev_volume	; set volume of last access to 254
	lda	#254
	sta	(rwts_iopb),y

	ldy	#rwts_iopb_drive_num	; set drive number of last access
	lda	(rwts_iopb),y
	ldy	#rwts_iopb_prev_drive
	sta	(rwts_iopb),y

	ror
	ror
	sta	Z35

	ldy	#rwts_iopb_track	; get logical track number from RWTS IOPB
	lda	(rwts_iopb),y
	sta	bubble_track		; save as bubble track number

	sec			; if track > 3, subtract 3 from bubble track
	sbc	#$03
	bpl	Lc890
	lda	bubble_track
Lc890:	sta	bubble_track

	tay			; bubble track >= 32?
	sec
	sbc	#$20
	bpl	rwts_finish

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

	ldy	#rwts_iopb_sector	; get sector number from RWTS IOPB
	lda	(rwts_iopb),y

	tay			; deinterleave
	lda	Dcac8,y

	ldy	bubble_track
	bne	Lc8b8

	tay
	lda	Dcae8,y
	bpl	Lc8c7

Lc8b8:	tay
	lda	Dcad8,y

	ldy	bubble_track
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

rwts_retry:
	jsr	initial
	bcs	rwts_check_status

Lc8dc:	ldy	#rwts_iopb_command	; get IOPB command code
	lda	(rwts_iopb),y
	beq	rwts_finish		; cmd 0 = SEEK, handle as no-op

	lsr
	bcc	Lc8eb			; cmd 2 or 4

; cmd 1 = READ
	jsr	rdbub
	jmp	rwts_check_status

Lc8eb:	lsr
	bcc	rwts_finish		; cmd 4 = FORMAT, handle as no-op

; cmd 2 = WRITE
	jsr	wptest
	bcc	Lc8f7			; if write protected, error
	lda	#$10
	bne	rwts_done_err

Lc8f7:	jsr	inbitmap
	jsr	wrbub

rwts_check_status:
	bcc	rwts_finish		; if no error, done

	pla				; decrement retry count on stack
	sec
	sbc	#$01
	pha

	bne	rwts_retry		; if retry count hasn't hit 0, retry

	beq	rwts_drive_error	; otherwise report drive error

rwts_finish:
	ldy	#rwts_iopb_dct_ptr	; copy IOPB DCT ptr to Z3c
	lda	(rwts_iopb),y		; (presumably for RWTS compatibility)
	sta	rwts_dct
	iny
	lda	(rwts_iopb),y
	sta	rwts_dct+1

	jsr	click
	lda	#$00
	beq	rwts_done_ok

rwts_drive_error:
	lda	#$40	; drive error
	bne	rwts_done_err

rwts_done_ok:
	clc
	fcb	$24	; bit instr to skip sec
rwts_done_err:
	sec
	ldy	#rwts_iopb_return_code
	sta	(rwts_iopb),y
	lda	led_off,x

	pla		; pop retry count from stack
	rts


; check whether 7220 BMC needs initializing
initchk:
	lda	#bmc_cmd_reset_fifo
	sta	bmc_cmd,x
	jsr	cmdwait
	rts


; wait for BMC to go busy, then idle
cmdwait:
	lda	#$00
	pha
Lc936:	lda	bmc_status,x
	bmi	Lc940
	dey
	beq	Lc94f
	bne	Lc936

Lc940:	lda	bmc_status,x
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
	sta	bmc_cmd,x
	ldx	#$00

	tya
	sta	(Z2c,x)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z2c,x)		; write to block length MSB

	lda	#$28		; enable RCD (read corrected data)
				;   and MFBTR (max FSA-BMC transfer rate)
	sta	(Z2c,x)		; write to enable register

	lda	Z2e
	sta	(Z2c,x)		; write to address register LSB

	lda	Z2f
	sta	(Z2c,x)		; write to address register MSB

	ldx	dos_slot_16
	rts


; send block length and address to BMC for commands other than read data,
; including write with error correction
; used for DOS
; (disables MFBTR)
; on entry, Y = number of 64-byte pages for block length
ldwrrac:
	lda	#bmc_reg_block_length
	sta	bmc_cmd,x
	ldx	#$00

	tya
	sta	(Z2c,x)		; write to block length LSB

	lda	#$10		; two FSA channels
	sta	(Z2c,x)		; write to block length MSB

	lda	#$20		; enable RCD (read corrected data) only
	sta	(Z2c,x)		; write to enable register

	lda	Z2e
	sta	(Z2c,x)		; write to address register LSB

	lda	Z2f
	sta	(Z2c,x)		; write to address register MSB

	ldx	dos_slot_16
	rts


; DOS: set Z2c to point to data reg
dos_setup_data_reg_ptr:
	lda	dos_slot_16
	clc
	adc	#$80
	sta	Z2c
	lda	#$c0
	sta	Z2c+1
	rts


; initialize: tell BMC to abort, twice
initial:
	lda	#bmc_cmd_abort
	sta	bmc_cmd,x
	jsr	cmdwait
	bcs	Lc9dc

	lda	#bmc_cmd_abort
	sta	bmc_cmd,x
	jsr	cmdwait
	bcs	Lc9dc

	lda	bmc_status,x
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
	sta	bmc_cmd,x
	jsr	cmdwait
	bcs	Lc9dc

	lda	bmc_status,x
	cmp	#$40
	bne	Lc9dc
	clc
	fcb	$24	; bit instr to skip sec
Lc9dc:	sec
	rts


Sc9de:	lda	#$ff
	pha
Lc9e1:	lda	bmc_status,x
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
Lc9f6:	lda	bmc_status,x
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
	lda	Dc08c,x
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
	sta	bmc_cmd,x
	jsr	Sc9de
	bcs	Lca52

; read data loop
Lca36:	jsr	Sc9f2
	bcs	Lca47
	ldx	#$00
	lda	(Z2c,x)
	ldx	dos_slot_16
	sta	(Z26),y
	iny
	jmp	Lca36

Lca47:	beq	Lca52
	lda	bmc_status,x
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
	sta	bmc_cmd,x
	jsr	Sc9de
	bcs	Lca84

; write data loop
Lca68:	jsr	Sc9f2
	bcs	Lca79
	lda	(Z26),y
	iny
	ldx	#$00
	sta	(Z2c,x)
	ldx	dos_slot_16
	jmp	Lca68

Lca79:	beq	Lca84
	lda	bmc_status,x
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
	lda	bubble_track	; is VTOC track (17, adjusted for bubble)?
	cmp	#17-3
	bne	inbitmap_done

	ldy	#rwts_iopb_sector	; VTOC sector (0)?
	lda	(rwts_iopb),y
	cmp	#$00
	bne	inbitmap_done

	jsr	get_dos_fm_parm
	sty	Z2c
	sta	Z2c+1
	ldy	#$00
	lda	(Z2c),y
	cmp	#$0b
	bne	inbitmap_done
	lda	Z35
	bpl	inbitmap_done
	lda	#$fe
	ldy	#$06
	sta	(Z26),y
	lda	#$00
	ldy	#$44
Lcab1:	sta	(Z26),y
	iny
	cpy	#$4c
	bne	Lcab1
	iny
	lda	#$e0
	sta	(Z26),y

inbitmap_done:
	rts


; toggle speaker if enabled by switch
click:	lda	Dc08c,x
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

Lcb21:	lda	led_on,x
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
	lda	led_off,x
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
	lda	Dc08e,x
Lcbec:	ldy	#$08
	lda	Dc08c,x
Lcbf1:	cmp	Dc08c,x
	bne	Lcbec
	dey
	bne	Lcbf1
	pla
	tax
Lcbfb:	lda	led_on,x
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
	lda	Dcca9,y
	ldy	D03e0
	bne	Lcc4b
	tay
	lda	Dcae8,y
	bpl	Lcc4f
Lcc4b:	tay
	lda	Dcad8,y
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
	lda	led_off,x
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
	sta	prodos_prev_unit
	lda	#$04
	pha
	ldx	Z3e
	lda	led_on,x
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
Lcd52:	lda	led_off,x
	jsr	click
	ldx	#$00
	ldy	#$01
	pla
	rts

Scd5e:	eor	prodos_prev_unit
	and	#$7f
	beq	Lcd84
	lda	prodos_prev_unit
	and	#$0f
	bne	Lcd84
	lda	prodos_prev_unit
	and	#$70
	tax
	beq	Lcd84
	lda	Dc08e,x
Lcd77:	ldy	#$08
	lda	Dc08c,x
Lcd7c:	cmp	Dc08c,x
	bne	Lcd77
	dey
	bne	Lcd7c
Lcd84:	rts


; send block length and address to BMC for read data command
; (enables MFBTR, can't be used for commands other than read data)
setup_prodos_read:
	lda	#bmc_reg_block_length
	sta	bmc_cmd,x
	ldx	#$00

	lda	#$08			; 8*64 = 512 byte block
	sta	(prodos_bmc_ptr,x)	; write to block length LSB

	lda	#$10			; two FSA channels
	sta	(prodos_bmc_ptr,x)	; write to block length MSB

	lda	#$28			; enable RCD (read corrected data)
					;   and MFBTR (max FSA-BMC transfer rate)
	sta	(prodos_bmc_ptr,x)	; write to enable register

	lda	prodos_block
	sta	(prodos_bmc_ptr,x)	; write to address register LSB

	lda	prodos_block+1
	sta	(prodos_bmc_ptr,x)	; write to address register MSB

	ldx	prodos_slot_16
	rts


; send block length and address to BMC for commands other than read data
; (disables MFBTR)
setup_prodos_nonread:
	lda	#bmc_reg_block_length
	sta	bmc_cmd,x
	ldx	#$00

	lda	#$08			; 8*64 = 512 byte block
	sta	(prodos_bmc_ptr,x)	; write to block length LSB

	lda	#$10			; two FSA channels
	sta	(prodos_bmc_ptr,x)	; write to block length MSB

	lda	#$20
	sta	(prodos_bmc_ptr,x)	; write to enable register

	lda	prodos_block
	sta	(prodos_bmc_ptr,x)	; write to address register LSB

	lda	prodos_block+1
	sta	(prodos_bmc_ptr,x)	; write to address register MSB

	ldx	prodos_slot_16
	rts


; set prodos_bmc_ptr to point to BMC data reg
prodos_setup_data_reg_ptr:
	lda	prodos_slot_16
	clc
	adc	#$80
	sta	prodos_bmc_ptr
	lda	#$c0
	sta	prodos_bmc_ptr+1
	rts


Scdcd:	sei

	lda	#bmc_cmd_abort
	sta	bmc_cmd,x
	jsr	cmdwait
	bcs	Lce11

	lda	#bmc_cmd_abort
	sta	bmc_cmd,x
	jsr	cmdwait

	cli

	bcs	Lce11
	lda	bmc_status,x
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
	sta	bmc_cmd,x
	jsr	cmdwait

	cli
	bcs	Lce11
	lda	bmc_status,x
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
	sta	bmc_cmd,x
	jsr	Sc9de
	bcs	Lce46

; read data loop
Lce26:	jsr	Sc9f2
	bcs	Lce3b
	ldx	#$00
	lda	(prodos_bmc_ptr,x)
	ldx	Z3e
	sta	(prodos_buf_ptr),y
	iny
	bne	Lce26
	inc	prodos_buf_ptr+1
	jmp	Lce26

Lce3b:	beq	Lce46
	lda	bmc_status,x
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
	sta	bmc_cmd,x
	jsr	Sc9de
	bcs	Lce7c

; write data loop
Lce5c:	jsr	Sc9f2
	bcs	Lce71
	lda	(prodos_buf_ptr),y
	iny
	bne	Lce68
	inc	prodos_buf_ptr+1
Lce68:	ldx	#$00
	sta	(prodos_bmc_ptr,x)
	ldx	prodos_slot_16
	jmp	Lce5c

Lce71:	beq	Lce7c
	lda	bmc_status,x
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
Lce89:	lda	Dce7f,x
	cmp	D0800,x
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
