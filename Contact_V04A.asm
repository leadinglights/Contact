

	;;Version history
; V04 replaces V03 as the baseline mAD calculations were unrecoverably screwed
; In this version the oldesr 112 readings are used to establish a true running
; average to get a redictable baseline. The same values are used to get a good
; Mean Average Deviation.
; Response to various good and bad signals has changed so that any kind of bad
; signal is treated the same - i.e. a requirement to reprobe. The LEDs on the
; other hand can be used for diagnosis. A green LED alone signals a good
; contact detection, An amber is only used for backstop value hit while both
; green and amber signal that the values have been met for a good contact but
; some other measurement was wrong - baseline out of limits, ramp not linear
; or rise time too fast or slow.
; V03 Changes MCU to PIC16F1704 for larger data storage
; Contact is a rebuild of Piezo14 following from the earlier Piezo14 software.
; In the earliest tests the primary objectives were met but there was 
; unanticipated sensitivity to mechanical noise transmitted through the frame
; of the printer.

; Assembly source line config statements

 #include "p16f1704.inc";  ### much wierdness on debug  #####
; CONFIG1
; __config 0x3FE4
 __CONFIG _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _BOREN_ON & _CLKOUTEN_OFF
; CONFIG2
; __config 0x3FFF
 __CONFIG _CONFIG2, _WRT_OFF & _PPS1WAY_ON & _ZCDDIS_ON & _PLLEN_ON & _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON

	Errorlevel 	0,-302
	Errorlevel 	0,-303	
 ; !!!!!!!!!!!!! WDT switched off for simulation !!!!!!!!!!!!
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

Array0	UDATA	0x020
	RES	0x50
Array1	UDATA	0x0A0
	RES	0x50
Array2	UDATA	0x120
	RES	0x50
Array3	UDATA	0x1A0
	RES	0x50
	
; Other parts of linear memory on PIC16F1704 are not used
	
; Variables
	
	UDATA_SHR
OutTim	RES	1	; Number of read cycles for valid signal
DataPtr	RES	1	; Uncorrected pointer to ADC buffer
Temp1	RES	2	; Temporary stores
BaseTot	RES	3	; Total for running average
BaseLin	RES	2	; Baseline at X8 scale
TrigTot	RES	2	; Total for limit trigger
TFirst	RES	2	; First value above T1
BaseMAD	RES	2	; baseline Mean Absolute Deviation
TCount	RES	1	; number of cycles between T1 and T2
	
RES_VECT  CODE	0x0000	; processor reset vector
  
	GOTO    START	; go to beginning of program
  
INT_VECT	CODE	0x0004		; Context saving active on 1704
	bcf	INTCON,TMR0IF	; only active interrupt
	decfsz	OutTim,F	
	retfie
	bcf	INTCON,TMR0IE
	retfie

	GOTO    START                   ; go to beginning of program
	
; Test constants for getting trigger values

	; CycTim is Timer2 used for ADC rate,
	; SigTim is number Timer0 cycles following a contact event 
	; detection for which no further contact will be sought and
	; for which the LED will be held on.

CycTim	EQU	0x05		; Time per reading = N * 32us (was 4)
	
	; *** Calibration 0x1E gave 1ms per sample
	; 0x0F gave a little over 500us per sample
	; 0x0A gave 350us
	
SigTim	EQU	0x40		; 400us for LEDs etc. (was 0x62)	
LoBL	EQU	0x3E00		; 15872 (32 value total)
HiBL	EQU	0x4100		; 16640      

; The trigger test limits are 8 value limits scaled to the 32 value.
; Backstop is the pure ADC value which is used to unconditionally trigger
; the contact signal without the good signal. 
; and subtracted from the current 32 value total
	
; New values to get around excess sensitivity
	
Backstop	EQU	0x1000
T1	EQU	0x0010		; Can be in noise band
T2	EQU	0x0060		; Above the first value > T1
Spike	EQU	0x00A0		; above credible first value
Bend	EQU	0x0040		;!!!!TBD
LoMAD	EQU	0x0800		; !!!!TBD
MaxT	EQU	0x20		; !!! TBD !!!!
SafeMAD	EQU	0x3A98		; !!!TBD
	
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
	
	; OPTION_REG
;  bit 7      0	Not used here
;  bit 6      0	Not used here
;  bit 5      0	Timer0 source is Fosc/4
;  bit 4      0	Not used here
;  bit 3      0	Prescaler to Timer0
; bits 2-1  101	prescaler is 1:64 (4.096ms per tick)
	
	movlw	b'10000101'
	movwf	OPTION_REG
	
	clrf	INTCON		; is a core register
	bsf	INTCON,GIE
	banksel	WDTCON
	movlw	b'00010100'	
	movwf	WDTCON; 1 second

; Initialisation of working memory - clear shared memory
	
	clrf	OutTim
	clrf	DataPtr
	clrf	Temp1
	clrf	Temp1+1
	clrf	BaseTot
	clrf	BaseTot+1
	clrf	BaseTot+2
	clrf	TrigTot
	clrf	TrigTot+1
	clrf	BaseLin
	clrf	BaseLin+1
	clrf	BaseMAD
	clrf	BaseMAD+1
;	clrf	BCurve
;	clrf	BCurve+1

; Clear the linear memory

	clrf	FSR0L
	movlw	0x20
	movwf	FSR0H
Clr1	clrf	INDF0
	decfsz	FSR0L,F
	goto	Clr1
	movlw	0x3F
	movwf	FSR0L
	movlw	0x21
	movwf	FSR0H
Clr2	clrf	INDF0
	decf	FSR0L,F
	btfss	FSR0L,7		; skip when below 0
	goto	Clr2
	
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
	; bits 6-4 = 010 Fosc/32
	; bit    3 = 0 unimplemented
	; bit    2 = 0 Vref- connected to Vss
	; bits 1,0 = 11 Vref+ connected to FVR_Buffer   
    
	banksel	ADCON1
	movlw	b'10100000'
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
	movlw	b'00001011'	; postscaler = 10
	movwf	T2CON
	movlw	CycTim
	movwf	PR2		; Period register
	bsf	T2CON,TMR2ON	; Start Timer2	
	movlw	0x21
	movwf	FSR0H
	movlw	0x20
	movwf	FSR1H
	clrf	DataPtr		; Dataptr was used for clearing arrays
Loop1
	movf	DataPtr,W
	andlw	b'00111111'		; 32 values (64 bytes)	
	movwf	FSR0L
	banksel	PIR1
	btfss	PIR1,TMR2IF
	goto	$-1
	bcf	PIR1,TMR2IF		; Look for Timer2 to set the flag
	banksel	ADCON0
	btfsc	ADCON0,ADGO
	goto	$-1		; Wait for ADC to be ready
	
; Complexity is due to only 112 of 128 samples being used.

	movf	BaseTot+2,W				
	movwf	Temp1+1				
	movwf	Temp1				
	lsrf	Temp1+1,F				
	lsrf	Temp1+1,F				
	lsrf	Temp1+1,W				
	addwf	Temp1,F											
	swapf	BaseTot+1,W				
	andlw	b'00001111'				
	movwf	Temp1+1				
	lsrf	Temp1+1,W				
	addwf	Temp1,F				
	swapf	BaseTot+2,W				
	andlw	b'11110000'			
	movwf	Temp1+1				
	lsrf	Temp1+1,W				
	addwf	Temp1,F											
	lsrf	BaseTot+2,W		; Get C
	rrf	BaseTot+1,W				
	movwf	Temp1+1				
	lsrf	Temp1+1,W				
	btfsc	BaseTot+2,1				
	iorlw	b'10000000'				
	addwf	Temp1,F		
	lsrf	BaseTot+2,W				
	movwf	Temp1+1				
	lsrf	Temp1+1,F							
	lslf	BaseTot,W		; Get ms bit from LS byte
	rlf	BaseTot+1,W				
	movwf	BaseLin
	rlf	BaseTot+2,W
	movwf	BaseLin+1										
	movf	Temp1,W				
	addwf	BaseLin,F				
	movf	Temp1+1,W				
	addwfc	BaseLin+1,F				
	movf	DataPtr,W
	addlw	0xE0		; Actually points back 16 elements	
	movwf	FSR1L
	moviw	0[FSR1]
	movwf	Temp1		; copy for MAD addition
	addwf	BaseTot,F
	moviw	1[FSR1]
	movwf	Temp1+1
	addwfc	BaseTot+1,F
	movlw	0
	addwfc	BaseTot+2,F

; Temp1 contains a copy of the 16th oldest element which is compared to the
; baseline to get the difference between that elelment and the baseline

	movf	BaseLin,W
	subwf	Temp1,F
	movf	BaseLin+1,W
	subwfb	Temp1+1,F
	btfss	Temp1+1,7
	goto	AddMAD
	comf	Temp1,F
	comf	Temp1+1,F
	incf	Temp1,F
	btfsc	STATUS,Z
	incf	Temp1+1,F

; Add the element difference to the baseline MAD total

AddMAD
	movf	Temp1,W
	addwf	BaseMAD,F
	movf	Temp1+1,W
	addwfc	BaseMAD+1,F

; now subtract the oldest value in the 128 element array - this is the
; value which will be replaced by the next TrigTot

	movf	DataPtr,W	; point to oldest (newest replaces)
	movwf	FSR1L
	moviw	0[FSR1]
	movwf	Temp1		; copy for MAD subtraction
	subwf	BaseTot,F
	moviw	1[FSR1]
	movwf	Temp1+1
	subwfb	BaseTot+1,F
	movlw	0
	subwfb	BaseTot+2,F

;  Get 1/128 of BaseMAD and add 1/7 of this to the result which should give
; should give 1/112 of BaseMAD. 1/7 = 1/8 + 1/64 + 1/512 
; After a contact even or other signal injection, BaseMAD may get too large
; so it should be limited to a save level

	movlw	LOW(SafeMAD)
	subwf	BaseMAD,W
	movlw	High(SafeMAD)
	subwfb	BaseMAD+1,W
	andlw	b'10000000'
	btfss	STATUS,Z
	goto	Simple
	movlw	LOW(SafeMAD)
	movwf	BaseMAD
	movlw	HIGH(SafeMAD)
	movwf	BaseMAD+1
Simple
	swapf	BaseMAD+1,W
	andlw	b'00001111'
	movwf	Temp1
	lsrf	Temp1,F
	lsrf	BaseMAD+1,W
	movwf	Temp1+1
	lsrf	Temp1+1,W
	addwf	Temp1,F
	clrf	Temp1+1
	lslf	BaseMAD,W
	rlf	BaseMAD+1,W
	lslf	Temp1+1,F
	addwf	Temp1,F
	btfsc	STATUS,C
	incf	Temp1+1,F
	subwf	BaseMAD,F
	movf	Temp1+1,W
	subwfb	BaseMAD+1,F

; Get the data from the ADC and put it in the array. Add this value to 
; the trigger total (TrigTot) at the same time
	
Mark1
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
	
	movf	FSR0L,W
	addlw	0x30		; points 8 back
	movwf	FSR0L
	bcf	FSR0L,6
	moviw	0[FSR0]
	subwf	TrigTot,F
	moviw	1[FSR0]
	subwfb	TrigTot+1,F
			
; FSR1L will still hold the value for the oldest element which will now
; be replaced by the value in trigTot.

	movf	TrigTot,W
	movwi	0[FSR1]		; in TrigTot array
	movf	TrigTot+1,W
	movwi	1[FSR1]

; All essential data has been stored so see if either good or bad
; contacts have occured - or if to keep looking
	
	btfsc	INTCON,TMR0IE	; If flag set then hold LED on 
	goto	Cycle		; just continue getting data
	banksel	LATA
	btfss	LATA,RA2		; if high then LED is on
	goto	TooHigh		; otherwise just continue 
	bcf	LATA,RA2
	bcf	LATA,RA5
	bsf	LATC,RC0		; turn off LEDs
	bsf	LATC,RC1
	goto	Cycle		; and get more
	
; Before seeking for good contacts it is neccessary to check failed 
; contacts - where the BackStop value is exceeded

TooHigh
	movlw	LOW(Backstop)
	subwf	TrigTot,W
	movlw	HIGH(Backstop)
	subwfb	TrigTot+1,W
	movlw	b'00000001'		; signal for backstop
	btfsc	STATUS,C
	goto	Amber		; Above BackStop limit

; The main seek routine when there is not blanking time (during LED)
; The value needed is present value of TrigTot above BaseLine and will
; be in Temp1
	
MainSeek
	movf	BaseLin,W
	subwf	TrigTot,W
	movwf	Temp1
	movf	BaseLin+1,W
	subwfb	TrigTot+1,W
	movwf	Temp1+1

; If T1 has already been passed then look for T2
	
	movf	TCount,F
	btfsc	STATUS,Z
	goto	SeekT1

; Check if Trigtot above first reading and cancel search for T2 if not

	movf	TFirst,W
	subwf	TrigTot,W
	movwf	Temp1
	movf	TFirst+1,W
	subwfb	TrigTot+1,W
	movwf	Temp1+1
	btfsc	STATUS,C	; TrigTot < TFirst if no C
	goto	UpSlope
	clrf	TCount
	clrf	TFirst
	clrf	TFirst+1
	goto	SeekT1	; Check if restart ramp
	
; TCount is non zero so T1 has been found, Increment it and check if ramp
; is too low then reset and keep looking for a new T1
	
UpSlope
	incf	TCount,F
	movlw	MaxT
	subwf	TCount,W
	btfss	STATUS,Z
	goto	SeekT2
	clrf	TCount
	goto	Cycle

; Changes here are that T2 becomes relative to the first passed value
; instead of relative to the baseline.

SeekT2
	movlw	LOW(T2)
	subwf	Temp1,W
	movlw	HIGH(T2)
	subwfb	Temp1+1,W
	andlw	b'10000000'
	btfss	STATUS,Z		; Negative if T2>Temp1
	goto	Cycle

; Three values are now needed, The value when T2 was passed, the value 
; when T1 was passed and the mid-point value. If the count between is odd
; then two points are used. To calculate if the mid-point is on a
; straight line the formula for even counts is E(T1) + E(T2) - 2 * E(TM)
; for odd counts this becomes E(T1) + E(T2) - E(TM) - E(TM - 1)
	
	movf	TrigTot,W
	movwf	Temp1
	movf	TrigTot+1,W
	movwf	Temp1+1
	lslf	TCount,W		; TCount X 2
	subwf	DataPtr,W
	movwf	FSR1
	moviw	0[FSR1]
	addwf	Temp1,F
	moviw	1[FSR1]
	addwfc	Temp1+1,F
	movf	TCount,W		; By 2 then div2 (-:
	andlw	b'11111110'		; mask off ls bit
	subwf	DataPtr,W
	movwf	FSR1
	moviw	0[FSR1]
	subwf	Temp1,F
	moviw	1[FSR1]
	subwfb	Temp1+1,F
	btfss	TCount,0
	goto	Evens
	movlw	2
	subwf	FSR1,F
Evens
	moviw	0[FSR1]
	subwf	Temp1,F
	moviw	1[FSR1]
	subwfb	Temp1+1,F		

; Check Temp1 for limits of strightness	
	
	btfss	Temp1+1,7
	goto	BendOK
	comf	Temp1+1,F
	comf	Temp1,F
	incfsz	Temp1,F
	goto	BendOK
	incf	Temp1+1,F
BendOK
	movlw	LOW(Bend)
	subwf	Temp1,W
	movlw	HIGH(Bend)
	subwfb	Temp1+1,W
	andlw	b'10000000'
	btfss	STATUS,Z
	movlw	b'00000010'
	goto	Report
	
	; Look for T1 (first trigger point). Entered with
	; Temp1 = TrigTot - BaseLin
SeekT1
	movlw	LOW(T1)
	subwf	Temp1,W
	movlw	HIGH(T1)
	subwfb	Temp1+1,W	; Keep Temp 1 to look for Spike
	andlw	b'10000000'
	btfss	STATUS,Z
	goto	Cycle	; No trigger found
	
; Check for value above credible first reading. This will be
; a high transient which will disrupt baseline seeking so the
; timer used for LEDs is used to blank out seeking for this time

	movlw	LOW(Spike)
	subwf	Temp1,W
	movlw	HIGH(Spike)
	subwfb	Temp1+1,W
	andlw	b'10000000'
	btfss	STATUS,Z	; Equal or positive if T1<Temp1
	goto	Gap
	movlw	b'00000011'	; no LEDs
	goto	Report
	
; Start the cycle counter between T1 and T2 and copy TrigTot into TFirst

Gap
	movf	TrigTot,W
	movwf	TFirst
	movf	TrigTot+1,W
	movwf	TFirst+1 
	movlw	1
	movwf	TCount
	goto	Cycle

; Turn on lEDS and signals to host depending on entry
; If backstop then amber LED, signal contact but don't signal quality
; If contact passes all tests than green LED and quality signal
; If contact within high & low limits but not linear then both LEDS
; but no quality signal
; If signal is too rapid, too slow or bad baseline then contact but
; no quality signal
; 
; Entry with code in W
; W=00 Both LEDs, no Quality signal 
; W=01 Amber LED no Quality signal
; W=10 Green LED, send Quality signal
; W=11 No LEDs, no quality signal but blanking period still needed

Report
	banksel	LATC
	bsf	LATA,RA2	; Send contact seen
	andlw	b'00000011'	; for safety, only bits that count
	brw
	goto	Both
	goto	Amber
	goto	Green
	goto	GoTmrs
Both
	bcf	LATC,RC0
Amber
	bcf	LATC,RC1
	goto	GoTmrs
Green
	bcf	LATC,RC0
	bsf	LATA,RA5
GoTmrs
	clrf	TCount	; clear for next ramp
	movlw	SigTim
	movwf	OutTim
	banksel	TMR0
	clrf	TMR0	; Start Timer0
	bsf	INTCON,TMR0IE
	clrf	TCount	; Clean upfor next detection
	clrf	TFirst
	clrf	TFirst+1	
Cycle
	incf	DataPtr,F
	incf	DataPtr,F
	goto	Loop1

	END