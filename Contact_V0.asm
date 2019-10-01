; as at Sep 29 11:45
    
;;Version history
	
; V12
; This is working for overpressure threshold, upper and lower thresholds
;  amd slope and curve thresholds in simulation so is reasy for use on 
; Pickit.
	
; V10 The slope calculation using SG 9 point differentiation has proven
; unsatisfactory and does not produce a useful basis for assessing if the
; signal rate rise is linear. A combination of bounce at the nozzle and
; the available time has necessitated a change in the calculation

; the new method uses only one single array of 80 16 bit values with the
; values of the 8 bit trigger array and the baseline array being extracted
; from segments on the array.
	
; The slope and curve are calculated from a secondary array of the last 8
; values of the trigger totals (TrigTot). 3, 5 and 7 points will be tried with
; slope being the value of the newest element minus the value of the oldest
; element and the curve being the sum of the newest and oldest element
; minus twice the value of the midpoint element.	

; V9 replaces the array of the 16 most recent values with two arrays, the
; most recent 8 values for TrigTot and slope and the next most recent 8 values
; to act as an "ignore" buffer
	
; V6 introduces a delay between the TrigTot and BaseTot arrays of 8 readings. This
; is because the point of inflexion would otherwise be part of either of the
; above arrays. The delay is achieved by moving the array for the slope array
; to a higher address and enlarging the TrigTot arraywith only the most recent
; 8 readings being used for the TrigTot variable - the other 8 readings become
; a delay line.

; This version for PIC16F1703 with 256 bytes of data memory. The size of the 
; baseline data buffer and MAD buffer is only 32 values each vs 64 values in
; the PIC16F1704 version
	
;; PIC16F1704 version which will run on MPLab 8.92 and Pickit3 primarily for
;; compatibility with MPLab 8.92
	
; Piezo14 is an update of Piezo 8 but uses a 14 pin MCU with built in
; Op-amps and separate outputs for the status LEDs for signals to the
; host controller.
	
; PIC16F1704 include file and Configuration bits from configuration 
; bits generator
	
; PIC16F1704 Configuration Bit Settings

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
 
; V10 replaces the previous arrays with only two, both addressed through
; The linear indirect mode. The main array for the baseline and trigger
; calculations occupies 160 bytes from 0x2000 to 0x209F while an eight
; value array at 0x20A0 to0x20AF holds the last trigger values for 
; Slope and Curve calculations

Array1	UDATA	0x20
	RES	0x50
	
Array2	UDATA	0xA0
	RES	0x50
	
Array3	UDATA	0x120
	RES	0x50

; Other parts of linear memory on PIC16F1704 are not used
	
; Variables
	
	UDATA_SHR
OutTim	RES	1	; Number of read cycles for valid signal
DataPtr	RES	1	; Uncorrected pointer to ADC buffer
Temp1	RES	2	; Temporary stores
BaseTot	RES	2	; Total for running average
TrigTot	RES	2	; Total for limit trigger
Slope	RES	2	; Slope near trigger point
Curve	RES	2	; Diference from non linear
MAD	RES	2	; Mean Absolute Deviation
Peak	RES	2	; Maximum deviation

	GLOBAL	Slope, TrigTot, BaseTot
	
RES_VECT  CODE	0x0000	; processor reset vector

	GOTO    START                   ; go to beginning of program
	
T0Run	EQU	0	; bit 0 of flags shows timer running

; TODO ADD INTERRUPTS HERE IF USED
; Need some interrupt service for I2C
	
; Test constants for getting trigger values

	; CycTim is Timer2 used for ADC rate,
	; SigTim is number of ADC cycles for which the output
	; signal and LEDs will be held.

; CycTim	EQU	0x38		; Time per reading = N * 32us (was 4)
CycTim	EQU	0x0A		; Time per reading = N * 32us (was 4)
	
	; *** Calibration 0x1E gave 1ms per sample
	; 0x0F gave a little over 500us per sample
	; 0x0A gave 350us
	
SigTim	EQU	0x80		; 128 * CycTim	
    

OV8	EQU	0x1080	  ; 4016  (8 value total)
LoBL	EQU	0x3E00	  ; 15872 (32 value total)
HiBL	EQU	0x4100	  ; 16640      
MADMax	EQU	0x0080	  ; 128 Maximum Mean Absolute Deviation
PeakLim	EQU	0x1000	  ;     Peak deviation

; The trigger test limits are 8 value limits scaled to the 32 value
; and subtracted from the current 32 value total

Trig8Lo	EQU	0x0020	  ; 64
Trig8Hi	EQU	0x0100	  ; 128
LoSlope	EQU	0x0080	  ; 
HiSlope	EQU	0x0400	  ; 
Bend	EQU	0x20	  ; 256 Deviation from linearity
	
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
	clrf	Slope
	clrf	Slope+1
	clrf	Curve
	clrf	Curve+1
	clrf	MAD
	clrf	MAD+1
	clrf	Peak
	clrf	Peak+1

; all this should work in any bank
	
; The actual arrays consist of four distinct areas in the linear data memory
; space divided in a way that allows simple roll-over of the pointers to each
; array. At the begining of the linear data space, the first 64 bytes from 
; 0x2000 to 0x203F store the 32 value baseline array. The second 64 bytes
; from 0x2040 to 0x207F holds the array of differences between the running
; mean and each data point for the Mean Absolute Deviation calculation
;
; The data memory from 0x2080 to 0x208F stores the 8 values for calculating
; the slope at the trigger point which, along with the latest value held in
; ADRESL & ADRESH are used to do this calculation.
;	
; The data memory from 0x2090 to 0x209F is used along with the last Slope
; value to calculate the Curve (the linearity of the trigger slope.
;
; The pointers for the arrays is formed by left shifting the DataPtr value
; to multiply it by 2 and masking off the two most significant bits which
; cycle from 0x00 to 0x3E. This is used in IND1FL as a pointer to the
; baseline array with 0x020 in INDF1H. Setting bit 4 in INDF1L will point
; to the MAD array.
;
; The pointer to the Slope and Curve Arrays are held in INDF0, the ms part
; in INDF0H is set to 0x20 and the lower part, INDF0L is a copy of INDF1L
; masked with the ms nibble cleared (b'0000xxxx') for the slope array and bit
; 4 set (b'0001xxxx') set to point to the Slope and Curve arrays.
;
; During the slope and curve calculations a running mean of the last 8 points
; plus the latest datum is used to obtain both a normal and high trigger
; value.
;
; The baseline calculation is made by adding the latest datum and subtracting
; the first (N=N-32) datum from a moving total then dividing by 32 (three 16
; bit left shifts) The MAD (mean absolute deviation) is obtained by adding 
; the absolute difference between the latest reading and the baseline. It is
; not necessary to divide the MAD by the number of samples as it only needs
; to be compared with a constant for good/bad tests

; Clear the linear memory

	movlw	0x20		; non shared data memory is linear
	movwf	FSR1H		; baseline and MAD arrays
	movwf	FSR0H		; and slope and curve arrays
	movlw	0xF0		; 240 bytes
	movwf	DataPtr
	movlw	0
	movwf	FSR1L
WipeLin	clrf	INDF1
	incf	FSR1,F
	decfsz	DataPtr,F
	goto	WipeLin
	
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

	clrf	DataPtr		; Dataptr was used for clearing arrays

Loop1	movf	DataPtr,W
	sublw	0xA0
	btfss	STATUS,Z
	movf	DataPtr,W
	movwf	DataPtr
	bcf	STATUS,C
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
	
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
; Subtract the value pointed at in the array from the baseline total.

	moviw	0[FSR0]
	subwf	BaseTot,F
	moviw	1[FSR0]
	subwfb	BaseTot+1,F
	nop
	
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
	
	movlw	0x10
	subwf	DataPtr,W
	btfss	STATUS,C
	addlw	0xA0
	movwf	FSR0L
	moviw	0[FSR0]
	subwf	TrigTot,F
	moviw	1[FSR0]
	subwfb	TrigTot+1,F
	nop
	
; Point to the total M (16?) back and add it to the baseline total
		
	movlw	0x20
	subwf	DataPtr,W
	btfss	STATUS,C
	addlw	0xA0
	movwf	FSR0L
	moviw	0[FSR0]
	addwf	BaseTot,F
	moviw	1[FSR0]
	addwfc	BaseTot+1,F
	nop
	
; Get the base address for the smoothed trigger values and put it into
; FSR1L, using this to copy the TrigTot value into the array from 0x20A0
	
	movlw	b'00001111'
	andwf	DataPtr,W
	iorlw	b'10100000'	;0xA0
	movwf	FSR1L
	movf	TrigTot,W
	movwi	0[FSR1]
	movf	TrigTot+1,W
	movwi	1[FSR1]
	
; Newest - Oldest = Slope
; Newest + Oldest - 2*Middle value = curve
; Work on basis that FSR1 is pointing to newest value
	
	moviw	0[FSR1]		; Get newest
	movwf	Slope
	movwf	Curve
	moviw	1[FSR1]
	movwf	Slope+1		; in Slope and Curve temporarily
	movwf	Curve+1

; Difference for Slope. With 7 point slope/curve oldest is next element,
; middle is oldest element plus 3
; for 5 point slope/curve, oldest is newest plus 3 and middle is oldest
; plus 2
	
; 	movlw	6		; This offset for 5 point
	
	movlw	2		; This offset for 7 point
	addwf	FSR1L,F
	bcf	FSR1L,4
	moviw	0[FSR1]
	subwf	Slope,F
	moviw	1[FSR1]
	subwfb	Slope+1,F
	
; Sum for part of Curve	
	
	moviw	0[FSR1]
	addwf	Curve,F
	moviw	1[FSR1]
	addwfc	Curve+1,F
	
; now subtract 2 * mid point (subtract two times)
	
;	movlw	4		; offset for 5 point
	
	movlw	6		; offset for 7 point
	addwf	FSR1L,F
	bcf	FSR1L,4
	moviw	0[FSR1]
	subwf	Curve,F
	moviw	1[FSR1]
	subwfb	Curve+1,F
	moviw	0[FSR1]
	subwf	Curve,F
	moviw	1[FSR1]
	subwfb	Curve+1,F	
	
; If result is negative then make it positive
	
	btfss	Curve+1,7
	goto	IsPos
	comf	Curve,F
	comf	Curve+1,F
	incfsz	Curve,F
	incf	Curve+1,F
IsPos	nop
	
; At this point the following should have been calculated:
;	The total of the last 8 points - used for the trigger point(s)
;	The total of the last 32 points
;	The slope between the most recent and an earlier trigger values
;	The curve between the most recent trigger value, the earlier
;	trigger value and a value at a midpoint
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
; If Curve > curve limit then exit 
; Else	
; 	Start timer
;	Turn on trigger signal
;	Turn on green LED.
; loop
	
; Check for timer running
; ****** In this version the number of read cycles is used for the time the 
; LED and signal will be on.
;
; If the count is zero then the delay has not been called and the tests must
; be called. If the count is non-zero then the data input and calculations
; must be collected but the tests are not useful so are bypassed

	banksel	LATA			; both LATs
	bcf	LATA,RA2
	bcf	LATA,RA5
	
	
	movf	OutTim,F
	btfsc	STATUS,Z
	goto	NoTim			; No timer called
	decfsz	OutTim,F
	goto	Cycle			; continue getting data
	
; Time out for LEDs
	
	bsf	LATC,RC0
	bsf	LATC,RC1
	goto	Cycle			; continue looking

; Check for the higher trigger does not need to be relative to
; baseline so can be simple compare

NoTim	movlw	LOW(OV8)
	subwf	TrigTot,W
	movlw	HIGH(OV8)
	subwfb	TrigTot+1,W	
	btfss	STATUS,C
	goto	NoOV		; TrigTot below OV trigger point
	
; Turn on amber led, signal host for over trigger and start count down
; **************** Use this point for detected Over Trigger **********

	movlw	SigTim
	movwf	OutTim
	banksel	LATC
	bcf	LATC,RC0	; Turn on amber LED
	bsf	LATA,RA2	; Contact, possibly OV seen so signal host
	goto	Cycle		; continue looking

; !!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ignore baseline checks for the moment

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

	movlw	HIGH(LoSlope)
	subwf	Slope+1,W
	btfss	STATUS,Z
	goto	HEq8
	movlw	LOW(LoSlope)
	subwf	Slope,W	
HEq8	btfss	STATUS,C
	goto	Cycle		; Slope is below low slope

; Check if slope is below high slope

	movlw	HIGH(HiSlope)
	subwf	Slope+1,W
	btfss	STATUS,Z
	goto	HEq9
	movlw	LOW(HiSlope)
	subwf	Slope,W	
HEq9	btfsc	STATUS,C
	goto	Cycle		; Slope is above high slope

; Check if curve within limits

	movlw	HIGH(Bend)
	subwf	Curve+1,W
	btfss	STATUS,Z
	goto	HEq10
	movlw	LOW(Bend)
	subwf	Curve,W
HEq10	btfsc	STATUS,C
	goto	Cycle		; Upstroke not linear

; **********  Breakpoint at next line only if all tests O.K., good signal
; Turn on amber led, signal host for over trigger and start timer

	movlw	SigTim
	movwf	OutTim
	banksel	LATC
	bcf	LATC,RC1	; Turn on green LED
	bsf	LATA,RA2	; Contact seen
	bsf	LATA,RA5	; and contact is good. send signal to host
	goto	Cycle		; continue looking

; Now that the ADC has been read it can be restarted so that it is ready
; by the next read

Cycle	incf	DataPtr,F
	incf	DataPtr,F
	goto	Loop1

	END





