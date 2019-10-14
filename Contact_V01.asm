;;Version history
; V01 
; Contact is a rebuild of Piezo14 following from the earlier Piezo14 software.
; In the earliest tests the primary objectives were met but there was 
; unanticipated sensitivity to mechanical noise transmitted through the frame
; of the printer.

; Assembly source line config statements

 #include "p16f1703.inc"
;  ### much wierdness on debug  #####
; CONFIG1
; __config 0x3FE4
 __CONFIG _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_ON & _CLKOUTEN_OFF
; CONFIG2
; __config 0x3FFF
 __CONFIG _CONFIG2, _WRT_OFF & _PPS1WAY_ON & _ZCDDIS_ON & _PLLEN_ON & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON

	Errorlevel 	0,-302
	Errorlevel 	0,-303	
 
; Two arrays are maintained, a circular array of 32 values of which the
; newest 8 form the trigger total (TrigTot) and the other 24 are a region
; which creates a delay before the oldest value is added onto the total
; for the baseline calculations Note that the earlier arrays for the
; the baseline are replaces BaseTot = BaseTot + Newest element - Oldest
; element with BaseTot = BaseTot + Newest element - BaseTot/64
;
; The second array has 32 elements each being a sum of 8 values (TrigTot)
; this is used to calculate the values to check against slope and curve
; windows

Array1	UDATA	0x20
	RES	0x40
Array2	UDATA	0xA0
	RES	0x40

; Other parts of linear memory on PIC16F1704 are not used
	
; Variables
	
	UDATA_SHR
OutTim	RES	1	; Number of read cycles for valid signal
DataPtr	RES	1	; Uncorrected pointer to ADC buffer
Temp1	RES	2	; Temporary stores
BaseTot	RES	2	; Total for running average
TrigTot	RES	2	; Total for limit trigger
TSlope	RES	2	; Slope near trigger point
TCurve	RES	2	; Diference from non linear
BSlope	RES	2	
BCurve	RES	2	

RES_VECT  CODE	0x0000	; processor reset vector

	GOTO    START                   ; go to beginning of program
	
T0Run	EQU	0	; bit 0 of flags shows timer running

; TODO ADD INTERRUPTS HERE IF USED
; Need some interrupt service for I2C
	
; Test constants for getting trigger values

	; CycTim is Timer2 used for ADC rate,
	; SigTim is number of ADC cycles for which the output
	; signal and LEDs will be held.

CycTim	EQU	0x05		; Time per reading = N * 32us (was 4)
	
	; *** Calibration 0x1E gave 1ms per sample
	; 0x0F gave a little over 500us per sample
	; 0x0A gave 350us
	
SigTim	EQU	0x80		; 128 * CycTim	
;OV8	EQU	0x1080		; 4016  (8 value total)
LoBL	EQU	0x3E00		; 15872 (32 value total)
HiBL	EQU	0x4100		; 16640      

; The trigger test limits are 8 value limits scaled to the 32 value.
; Backstop is the pure ADC value which is used to unconditionally trigger
; the contact signal without the good signal. 
; and subtracted from the current 32 value total
	
; New values to get around excess sensitivity
	

; Backstop	EQU	0x10E0		; 540 above gnd
Backstop	  EQU	0x1200
; Trig8Lo	EQU	0x0040		; 32 above baseline
Trig8Lo	  EQU	0x0080
; Trig8Hi	EQU	0x00A0		; 128
Trig8Hi EQU	0x0120
LoTSlope	EQU	0x0040
HiTSlope	EQU	0x00A0
LoBSlope	EQU	0xFFF0		; minus 16
HiBSlope	EQU	0x0020		; plus 32
TBend	EQU	0x08		; 
BBend	EQU	0x0A
	
; Timer 2 is used to set the rate at which the ADC is read and is 
; set by a constant in this implementation but will be an externaly
; settable value in the future.
; Timer1 is used to set the duration of the signal for the host 
; computer and the LEDs

START
 	clrf	PCLATH
	banksel	OSCCON
	movlw	0x78	; Set internal oscillator at 16MHz  
	movwf	OSCCON
	banksel	OPTION_REG
	movlw	b'10000111'
	movwf	OPTION_REG

; Initialisation of working memory - clear shared memory
	
	clrf	OutTim
	clrf	DataPtr
	clrf	Temp1
	clrf	Temp1+1
	clrf	BaseTot
	clrf	BaseTot+1
	clrf	TrigTot
	clrf	TrigTot+1
	clrf	TSlope
	clrf	TSlope+1
	clrf	TCurve
	clrf	TCurve+1
	clrf	BSlope
	clrf	BSlope+1
	clrf	BCurve
	clrf	BCurve+1

; Clear the linear memory

	movlw	0x20
	movwf	FSR0H
	movwf	FSR1H
	movlw	0x80		; 160 bytes
	movwf	DataPtr
ClrArray	clrf	INDF0
	incf	FSR0,F
	decfsz	DataPtr,F
	goto	ClrArray
	
; Port Initialisation
	
	; PORTA
	; RA0 = pin13, ICSPDAT, <> Pickit, I2CDAT <> Host
	; RA1 = pin12, ICSPCLK, < Pickit, I2CCLK < Host
	; RA2 = Signal recieved > Host
	; RA3 = NMCLR < Pickit
	; RA4 = Analog in from op-amps
	; RA5 = Signal Good > Host
	;
	; PORTC
	; RC0 = LED output Signal
	; RC1 = LED Signal Good
	; RC2 = OPA1OUT Reference voltage/ signal ground
	; RC3 = OPA2 Output to RA4
	; RC4 = OPA2IN-
	; RC5 = OPA2IN+  Input from Piezo sensors
	;
	; Note: Configure TRIS for Op-amp outputs as 1 to avoid 
	; digital IO circuitry conflicting with analog output 
	;
	; TRIS PORTA = 00011011
	; TRIS PORTC = 11111100
	;
	; ANSEL PORTA = 00010000  ACD Input on RA4 (AN3)
	; ANSEL PORTC = 00000000  (no other analog inputs)


	banksel	TRISA
	movlw	b'00011011'
	movwf	TRISA
	movlw	b'11111100'
	movwf	TRISC
	banksel	ANSELA
	movlw	b'00010000'
	movwf	ANSELA
	movlw	b'00000100'
	movwf	ANSELC
	banksel	PORTA
	clrf	PORTA
	clrf	PORTC
	banksel	LATC
	movlw	b'00000011'	; Bits RC0 and RC1 high = LEDS off
	movwf	LATC
	
	; FVRCON = 11000111
	; bit    7 = 1 Enabled
	; bit    6 = (Always 1) Voltage Ready
	; bit    5 = 0 Temperature indicator disabled
	; bit    4 = 0 Not used here
	; bits 3,2 = 10 Voltage output is 2.048V
	; bits 1,0 = 11 = 4 x 1.024V
	
	banksel	FVRCON
	movlw	b'11001011'
	movwf	FVRCON 

	; OPA1CON  = 11010011
	; bit    7 = 1 Enabled
	; bit    6 = 1 High GBWP mode
	; bit    5 = 0 (unimplemented)
	; bit    4 = 1 Unity gain
	; bits 3,2 = 00 (unimplemented)
	; bits 1,0 = 11 Non inverting input from FVR_Buffer2 output
	
	; OPA2CON  = 11000000
	; bit    7 = 1 Enabled
	; bit    6 = 1 High GBWP mode
	; bit    5 = 0 (unimplemented)
	; bit    4 = 0 Inverting input to OPA2IN- pin
	; bits 3,2 = 00 (unimplemented)
	; bits 1,0 = 00 Non inverting input to OPA2IN+ pin
	
	banksel	OPA1CON
	movlw	b'11010011'
	movwf	OPA1CON
	movlw	b'11000000'
	movwf	OPA2CON
	
	; ADCON0   = 00001101
	; Bit    7 = 0 not used
	; Bits 6-2 = 00011, Channel is AN3, (pin3, RA4) 
	; Bit    1 = GO/NDONE flag
	; Bit    0 = 1 for enable
 
	; ADCON1   = 10100011
	; bit    7 = 1 Right justified
	; bits 6-4 = 010 Fosc/64
	; bit    3 = 0 unimplemented
	; bit    2 = 0 Vref- connected to Vss
	; bits 1,0 = 11 Vref+ connected to FVR_Buffer   
    
	banksel	ADCON1
	movlw	b'10110000'
	movwf	ADCON1
	movlw	b'00001101'
	movwf	ADCON0

; Start the ADC ready for the first value
	
	nop
	bsf	ADCON0,ADGO

	; Code to set up Timer2 to give ca 100usec to 8192 usec input read
	; T2CON
	; bit  7   = x
	; bits 6-3 = 1001	  ; 10 was Postscaler =2
	; bit  2   = 0/1    Timer is off initially
	; bits 1-0 = 01	 ; 4 was Prescaler is 64

	banksel	T2CON
;	movlw	b'00001011'	; 520uS T2CON='00001011' CycTim = 0x1E
;	movwf	T2CON

	movlw	b'00001011'	; postscaler = 10
	movwf	T2CON
	movlw	CycTim
	movwf	PR2		; Period register
	bsf	T2CON,TMR2ON	; Start Timer2

; Timer 1 is used for the pulse to the host controller
;	T1CON bits 7,6 = 01 clocked at  Fosc (16Mhz)
;	rollover in 4.26ms
	
	banksel	T1CON
	movlw	b'01000000'	; source = osc
	movwf	T1CON
	clrf	T1GCON
	clrf	TMR1H
	clrf	TMR1L
	bcf	PIR1,TMR1IF	
	
	clrf	DataPtr		; Dataptr was used for clearing arrays

Loop1	movf	DataPtr,W
	movwf	FSR0L
		
	; Look for Timer2 to set the flag
	
	banksel	PIR1
	btfss	PIR1,TMR2IF
	goto	$-1
	bcf	PIR1,TMR2IF
	
; Start the ADC conversion

	banksel	ADCON0
	btfsc	ADCON0,ADGO
	goto	$-1	 ; Wait for ADC to be ready
	
; Subtract 1/64th of the baseline total from the baseline total. Division by
; 64 is easiest done by multiply by 8 then lose the MS byte
	
	clrf	Temp1+1
	movf	BaseTot+1,W
	movwf	Temp1
	rlf	BaseTot,W		; get MS bit of LS byte of BaseTot
	rlf	Temp1,F		; which now in LS bit of Temp1
	rlf	BaseTot+1,W	; get MS bit of MS byte of BaseTot
	rlf	Temp1+1,F		; which put in LS bit of Temp1+1
	btfsc	BaseTot,6
	bsf	STATUS,C
	rlf	Temp1,F
	rlf	Temp1+1,F
	
; and subtract
	
	movf	Temp1,W
	subwf	BaseTot,F
	movf	Temp1+1,W
	subwfb	BaseTot+1,F
		
; Add element pointed at in the array (oldest value) to the baseline total.
	
	movf	DataPtr,W
	movwf	FSR0L
	moviw	0[FSR0]
	addwf	BaseTot,F
	moviw	1[FSR0]
	addwfc	BaseTot+1,F

; Get the data from the ADC and put it in the array. Add this value to 
; the trigger total (TrigTot) at the same time
	
	movf	ADRESL,W
	movwi	0[FSR0]
	addwf	TrigTot,F
	movf	ADRESH,W
	movwi	1[FSR0]
	addwfc	TrigTot+1,F
	
; As the current value  from the ADC is no longer needed it is a good time
; to restart the ADC for the next conversion.

	banksel	ADCON0
	bsf	ADCON0,ADGO	
		
; Point to the data N (8?) back and subtract it from the trigger total
	
	movf	DataPtr,W
	addlw	0x30		; points 8 back
	movwf	FSR0L
	bcf	FSR0L,6
	moviw	0[FSR0]
	subwf	TrigTot,F
	moviw	1[FSR0]
	subwfb	TrigTot+1,F
			
; Get the base address for the smoothed trigger values and put it into
; FSR1L, using this to copy the TrigTot value into the array in Bank1
	
	movf	DataPtr,W
	addlw	0x40		; shouldn't need ms bit zeroing
	movwf	FSR1L
	movf	TrigTot,W
	movwi	0[FSR1]		; in TrigTot array
	movwf	TSlope		; and also in TSlope
	movwf	TCurve		; and TCurve
	movf	TrigTot+1,W
	movwi	1[FSR1]
	movwf	TSlope+1		; for ms bytes
	movwf	TCurve+1

; The trigger slope is calculated from the latest element and the element 
; 6 back. for convenience this is calculated from the element 26 forward.
; The trigger curve is calculated from the sum of the latest element and
; the one 6 back less twice the element 3 back, or 29 forward.
	
	movf	DataPtr,W
	addlw	0x74		; 64 + 52 for element N-6
	movwf	FSR1L
	movlw	0x40
	btfsc	FSR1L,7		; Superkludge
	subwf	FSR1L,F
	moviw	0[FSR1]
	subwf	TSlope,F
	moviw	1[FSR1]
	subwfb	TSlope+1,F
	
; Sum for part of TCurve	
	
	moviw	0[FSR1]
	addwf	TCurve,F
	moviw	1[FSR1]
	addwfc	TCurve+1,F
	
; now subtract 2 * mid point (subtract two times)

	movf	DataPtr,W
	addlw	0x7A		; 64 + 58 for element N-3
	movwf	FSR1L
	movlw	0x40
	btfsc	FSR1L,7		; Superkludge
	subwf	FSR1L,F
	moviw	0[FSR1]
	subwf	TCurve,F
	moviw	1[FSR1]
	subwfb	TCurve+1,F
	moviw	0[FSR1]		; and again 
	subwf	TCurve,F
	moviw	1[FSR1]
	subwfb	TCurve+1,F
	
; If result is negative then make it positive
	
	btfss	TCurve+1,7
	goto	IsPos
	comf	TCurve,F
	comf	TCurve+1,F
	incfsz	TCurve,F
	goto	IsPos
	incf	TCurve+1,F

; The values for the BSlope and BCurve are at N-18, N-21 and N-2
; Note that BSlope will be negative as often as positive
	
IsPos	movf	DataPtr,W
	addlw	0x60		; Element N-16
	movwf	FSR1L
	movlw	0x40
	btfsc	FSR1L,7		; Superkludge
	subwf	FSR1L,F
	moviw	0[FSR1]
	movwf	BSlope
	movwf	BCurve
	moviw	1[FSR1]
	movwf	BSlope+1
	movwf	BCurve+1
	
	movf	DataPtr,W
	addlw	0x6C		; Element N-22
	movwf	FSR1L
	movlw	0x40
	btfsc	FSR1L,7		; Superkludge
	subwf	FSR1L,F
	moviw	0[FSR1]
	subwf	BSlope,F
	moviw	1[FSR1]
	subwfb	BSlope+1,F	
	
	moviw	0[FSR1]
	addwf	BCurve,F
	moviw	1[FSR1]
	addwf	BCurve+1,F

	movf	DataPtr,W
	addlw	0x66		; Element N-19
	movwf	FSR1L
	movlw	0x40
	btfsc	FSR1L,7		; Superkludge
	subwf	FSR1L,F
	moviw	0[FSR1]
	subwf	BCurve,F
	moviw	1[FSR1]
	subwfb	BCurve+1,F
	moviw	0[FSR1]		; and again 
	subwf	BCurve,F
	moviw	1[FSR1]
	subwfb	BCurve+1,F	
	
; If result is negative then make it positive
	
	btfss	BCurve+1,7
	goto	IsOK
	comf	BCurve,F
	comf	BCurve+1,F
	incfsz	BCurve,F
	goto	IsOK
	incf	BCurve+1,F
	
; At this point the following should have been calculated:
;	The total of the last 8 points - used for the trigger point(s)
;	A 32 element array of the latest trigger point totals
;	A baseline total of the previous 64 ADC readings
;	The slope between the most recent and an earlier trigger values
;	The curve between the most recent trigger value, the earlier
;	trigger value and a value at a midpoint
;	The slope between two values 16 back in the trigger array
;	The curve between these values
; 
; Tests
; If time since last signal < time limit then exit
; If TrigTot > Over triger limit then
;	Start timer
; 	Set over limit signal on
;	Turn amber LED on
;	exit
; Turn off signals, LEDS and timer
;	
; If BaseTot < low baseline limit or BaseTot > high baseline limit then exit
; If TrigTot < low trigger limit then exit
; If Slope < lower slope limit or Slope > upper slope limit then exit
; If TCurve > curve limit then exit 
; Else	
; 	Start timer
;	Turn on trigger signal
;	Turn on green LED.
;
; If the count is zero then the delay has not been called and the tests must
; be called. If the count is non-zero then the data input and calculations
; must be collected but the tests are not useful so are bypassed

IsOK
	banksel	LATA			; both LATs
	bcf	LATA,RA2
	bcf	LATA,RA5	
	movf	OutTim,F
	btfsc	STATUS,Z
	goto	NoTim			; No timer called
	banksel	PIR1
	btfss	PIR1,TMR1IF
	goto	NoPulse
	bcf	T1CON,TMR1ON
	bcf	PIR1,TMR1IF
NoPulse
	decfsz	OutTim,F
	goto	Cycle			; continue getting data
	
; Time out for LEDs
	
	banksel	LATC
	bsf	LATC,RC0
	bsf	LATC,RC1
	goto	Cycle			; continue looking

; Check for the higher trigger does not need to be relative to
; baseline so can be simple compare

NoTim	movlw	LOW(Backstop)
	subwf	TrigTot,W
	movlw	HIGH(Backstop)
	subwfb	TrigTot+1,W	
	btfss	STATUS,C
	goto	NoOV		; below backstop trigger point
	
; Turn on amber led, signal host for over trigger and start count down

	goto	Amber

NoOV	lsrf	BaseTot+1,W
	movwf	Temp1+1
	rrf	BaseTot,W
	movwf	Temp1
	lsrf	Temp1+1,F
	rrf	Temp1,F
	lsrf	Temp1+1,F
	rrf	Temp1,F		
	
; Temp1 now contains BaseTot divided by 8 so subtract from TrigTot to give 
; present TrigTot above baseline

	movf	Temp1,W
	subwf	TrigTot,W
	movwf	Temp1
	movf	Temp1+1,W
	subwfb	TrigTot+1,W
	movwf	Temp1+1

; Difference in Temp1 so now compare with the lower trigger value
	
	movlw	LOW(Trig8Lo)
	subwf	Temp1,W
	movlw	High(Trig8Lo)
	subwfb	Temp1+1,W	
HEq6	andlw	b'10000000'
	btfss	STATUS,Z
	goto	Cycle		; Tot 8 below lower trigger point

; TrigTot is above Trig8Lo so possible good trigger but has it reached
; high value relative to baseline without a good trigger being found?

	movlw	HIGH(LoTSlope)
	subwf	TSlope+1,W
	btfss	STATUS,Z
	goto	HEq8
	movlw	LOW(LoTSlope)
	subwf	TSlope,W	
HEq8	btfss	STATUS,C
	goto	Cycle		; Slope is below low slope

; Check if slope is below high slope

	movlw	HIGH(HiTSlope)
	subwf	TSlope+1,W
	btfss	STATUS,Z
	goto	HEq9
	movlw	LOW(HiTSlope)
	subwf	TSlope,W	
HEq9	btfsc	STATUS,C
	goto	Cycle		; Slope is above high slope

; Check if curve within limits

	movlw	HIGH(TBend)
	subwf	TCurve+1,W
	btfss	STATUS,Z
	goto	HEq10
	movlw	LOW(TBend)
	subwf	TCurve,W
HEq10	btfsc	STATUS,C
	goto	Cycle		; Upstroke not linear

; Check if pre upstroke baseline segment slope is above low slope
 
	movlw	LOW(LoBSlope)
	subwf	BSlope,W		; only want carry
	movwf	Temp1
	movlw	HIGH(LoBSlope)
	subwfb	BSlope+1,W
	movwf	Temp1+1
	andlw	b'10000000'	; check for ms bit 
	btfss	STATUS,Z
	goto	Cycle	
	
; Check if pre upstroke baseline segment slope is below high slope

	movlw	LOW(HiBSlope)
	subwf	BSlope,W		; only want carry
	movwf	Temp1
	movlw	HIGH(HiBSlope)
	subwfb	BSlope+1,W
	movwf	Temp1+1
	andlw	b'10000000'	; check for ms bit 
	btfsc	STATUS,Z
	goto	Cycle
	
	
	
;	movlw	HIGH(HiBSlope)
;	subwf	BSlope+1,W
;	btfss	STATUS,Z
;	goto	BEq9
;	movlw	LOW(HiBSlope)
;	subwf	BSlope,W	
;BEq9	btfsc	STATUS,C
;	goto	Cycle		; Slope is above high slope
	
; Check if pre upstroke baseline segment curve is within limits

	movlw	HIGH(BBend)
	subwf	BCurve+1,W
	btfss	STATUS,Z
	goto	BEq10
	movlw	LOW(BBend)
	subwf	BCurve,W
BEq10	btfsc	STATUS,C
	goto	Cycle		; Upstroke not linear

; Turn on green led, signal host for contact and good and start timer

Green
	banksel	LATC
	bcf	LATC,RC1	; Turn on green LED
	bsf	LATA,RA2	; Contact seen
	bsf	LATA,RA5	; and contact is good. send signal to host
	goto	GoTmrs	; continue looking
	
Amber
	banksel	LATC
	bcf	LATC,RC0	; Turn on amber LED
	bsf	LATA,RA2	; Contact, possibly OV seen so signal host
	goto	Cycle		; continue looking

GoTmrs
	banksel	T2CON
	bsf	T2CON,TMR2ON	; Start timer2
	movlw	SigTim
	movwf	OutTim
	clrf	TMR1L
	clrf	TMR1H
	bsf	T1CON,TMR1ON
	
; Now that the ADC has been read it can be restarted so that it is ready
; by the next read

Cycle	incf	DataPtr,F
	incf	DataPtr,F
	btfsc	DataPtr,6
	clrf	DataPtr
	goto	Loop1

	END