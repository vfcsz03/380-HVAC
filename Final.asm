;***************************************************************************
;*
;* Title:   HVAC_sys_II_t3
;* Author:          Frank Yee
;* Version:         2.0
;* Last updated:    11/15/2015
;* Target:          ATmega16 
;*
;* DESCRIPTION
;* Interrupt INT1 is to be used to create an interrupt driven system. 
;* When INT1 is configured for positive edge triggering, an internal
;* flip-flop records an event at this input.
;* INT1 must be configured for positive edge triggering and 
;* interrupt INT1 and the global interrupt enabled
;* Under these conditions,if a key is pressed an INT1 interrupt occurs
;* The ISR reads the keycode from the 74C922 and places it in r16
;*
;***************************************************************************

;Simplified FSM - only allows 255 or fewer input characters
.nolist
.include "m16def.inc"
.list

.dseg						;data segment
humidicon_byte_1:.byte 1
humidicon_byte_2:.byte 1
humidicon_byte_3:.byte 1
humidicon_byte_4:.byte 1		;reserve for byte for humidicon
humidity_raw :.byte 2            ;reserve for byte for humidity_raw                
temperature_raw: .byte 2         ;reserve for byte for temperature_raw
temperature:.byte 2              ;reserve for byte for temperature
humidity:.byte 2                 ;reserve for byte for humidity  
temperature_setting:.byte 2		;reserve for byte for temp set
humidity_setting:.byte 2		;reserve for byte for hum set
temperature_unsaved:.byte 2		;reserve for byte for unsaved temp
humidity_unsaved:.byte 2		;reserve for byte for unsaved hum
pstate: .byte 2					;reserve for byte for FSM

measurandt: .byte 2			;reserve for byte for converted set T
measurandh: .byte 2			;reserve for byte for converted set H

button_value:	.byte 1		;reserve for byte for Pushbutton


.cseg		;start code segment

.org 0        ;location for reset
jmp main 

.org INT1addr	;insert vector for INT1 interrupt
jmp  isrread 

main: 
  ldi r16, HIGH(RAMEND)    ;initialize Stack Pointer
  out SPH, r16
  ldi r16, LOW(RAMEND)
  out SPL, r16
  
start:
;Configure port PA0 as an output, others inputs
    ldi r16, $f1       ;pin PA0 is output, others are inputs
    out DDRA, r16
    cbi PORTA,0        ;clear bit 0 of port A

;Configure port B as output port
ldi r16, $FF					;load r16 with all 1s
out DDRB, r16					;port B - all bits configured as outputs
sbi portB, 4                    ;set /SS of DOG LCD = 1 (Deselected)

;Configure port C as output for full-color led
	ldi r16, $02
	out DDRC, r16


;Configure port D	
    ldi r16,$81               ;PORTD, bit 6 as output, other as input
    out DDRD, r16   


;put FSM in initial state
    ldi r16, LOW(display_temp)	 ;initialize pointer to display_temp
    sts pstate, r16
	ldi r16, HIGH(display_temp)
	sts pstate+1, r16

	ldi ZL, LOW(temperature_setting)         ;initialize pointer to temperature_raw
    ldi ZH, HIGH(temperature_setting)
    ldi r16,$16
	ldi r17,$00

	st Z+,r16						;store low
    st Z, r17						;store high

	ldi ZL, LOW(humidity_setting)         ;initialize pointer to Humidity_setting
    ldi ZH, HIGH(humidity_setting)
    
	ldi r16,$32
	ldi r17,$00

	st Z+,r16					;store low
    st Z, r17					;store high

rcall init_lcd_dog              ;init display, using SPI serial interface
rcall clr_dsp_buff              ;clear all three buffer lines


  
   
  ldi r16, $0c		;config INT1 for pos edge 
  out MCUCR, r16
  ldi r16, $80		;enable INT1 for interrupt (1<<INT1)
  out GICR, r16
 
  sei				;globally enable interrupts
  



main_loop:
   
   	lds r16, button_value	;load state into register
	sbrc r16, 7				;Skip if Bit in Register is Set
	call fsm				;call FSM
	ldi r16,$00				;mask bit 7
	sts button_value, r16      ; store the button-value
  
  nop

   compare_temp:
   ldi r16, LOW(display_temp)	;initialize pointer to display temp
   ldi r17, HIGH(display_temp)


   lds r18, pstate		;load state into register
   lds r19, pstate+1

   cp r16, r18				;compare
   brne  compare_humid		;branch if not equal
   cp r17, r19				;compare
   brne  compare_humid		;branch if not equal
   call read_humidicon		;call subroutine read_humidicon 
   call temp_display		;call subroutine temp_display
   call humid_measure		;call subroutine humid_measure
   
   ldi ZL, LOW(temperature_setting)         ;initialize pointer to temperature_setting
   ldi ZH, HIGH(temperature_setting)
   ld r16, Z+
   ld r17, Z
	
	call convert			;convert hex to ASCII

   ldi ZL, LOW(measurandt)        ;initialize pointer to temp measurand 
   ldi ZH, HIGH(measurandt)

   ld r18, Z+			;load value into register
   ld r19, Z

   cp r21, r19			;compare
   brlo cooling			;Branch if Lower
   breq checknext		;Branch if not equal
   rjmp heating

   checknext:
   cp r10, r18		;compare
   brlo cooling		;Branch if Lower
   
   dec r10			;decrement
   cp r18,r10		;compare
   
   brlo heating		;Branch if Lower
   rjmp humidcp
   
 cooling:
 sbi PORTA, 6		;set bit
 cbi PORTA, 7		;clear bit
 rjmp humidcp

 heating:
 sbi PORTA, 7		;set bit
 cbi PORTA, 6		;clear bit


 humidcp:
   
   ldi ZL, LOW(humidity_setting)         ;initialize pointer to temperature_setting
   ldi ZH, HIGH(humidity_setting)
   ld r16, Z+
   ld r17, Z
  
  call convert

   ldi ZL, LOW(measurandh)        ;initialize pointer to temp measurand 
   ldi ZH, HIGH(measurandh)

   ld r18, Z+			;load value into register
   ld r19, Z
  
   cp r21, r19			;compare
   brlo dehumidifier	;Branch if Lower
   breq checknext1		;Branch if not equal
   rjmp humidifier

   checknext1:
   cp r10, r18			;compare
   brlo dehumidifier	;Branch if Lower
  
   dec r10				;decrement
   cp r18,r10			;compare
   
   brlo humidifier		;Branch if Lower
   rjmp main_loop
   
 dehumidifier:
 sbi PORTA, 4		;set bit
 cbi PORTA, 5		;clear bit
 rjmp last

 humidifier:
 sbi PORTA, 5		;set bit
 cbi PORTA, 4		;clear bit
 rjmp last
    
  compare_humid:
   ldi r16, LOW(display_humid)		;initialize pointer to display_humid
   ldi r17, HIGH(display_humid)

   lds r18, pstate		;load state into register
   lds r19, pstate+1

   cp r16, r18		;compare
   brne last			;branch if not equal
   cp r17, r19		;compare
   brne last		;branch if not equal
   call read_humidicon      ;call subroutine read_humidicon 
   call humid_display		;call subroutine temp_display
   call temp_measure		;call subroutine humid_measure

   ldi ZL, LOW(humidity_setting)         ;initialize pointer to temperature_setting
   ldi ZH, HIGH(humidity_setting)
   ld r16, Z+
   ld r17, Z
	
   call convert		;convert hex to ASCII
  
   ldi ZL, LOW(measurandh)        ;initialize pointer to hum measurand 
   ldi ZH, HIGH(measurandh)

   ld r18, Z+		;load value into register
   ld r19, Z
  
   cp r21, r19			;compare
   brlo dehumidifier1	;Branch if Lower
   breq checknext2		;Branch if not equal
   rjmp humidifier1

   checknext2:
   cp r10, r18			;compare
   brlo dehumidifier1	;Branch if Lower
  
   dec r10				;decrement
   cp r18,r10			;compare
   brlo humidifier1		;Branch if Lower
   rjmp tempcp
   
   dehumidifier1:
   sbi PORTA, 4			;set bit
   cbi PORTA, 5			;clear bit
   rjmp tempcp

   humidifier1:
   sbi PORTA, 5		;set bit
   cbi PORTA, 4		;clear bit

   tempcp:
   ldi ZL, LOW(temperature_setting)         ;initialize pointer to temperature_setting
   ldi ZH, HIGH(temperature_setting)
   ld r16, Z+
   ld r17, Z

   call convert
  
   ldi ZL, LOW(measurandt)        ;initialize pointer to temp measurand 
   ldi ZH, HIGH(measurandt)

   ld r18, Z+		;load value into register
   ld r19, Z

   cp r21, r19		;compare
   brlo cooling1	;Branch if Lower
   breq checknext3	;Branch if not equal
   rjmp heating1

   checknext3:
   cp r10, r18		;compare
   brlo cooling1	;Branch if Lower
   
   dec r10			;decrement		
   cp r18,r10		;compare
   brlo heating1	;Branch if Lower
   rjmp last
   
   cooling1:
   sbi PORTA, 6		;set bit
   cbi PORTA, 7		;clear bit
   rjmp last

    heating1:
   sbi PORTA, 7		;set bit
   cbi PORTA, 6		;clear bit
   

	last:
	call Full_ledclr
	nop
	rjmp main_loop        ; jump back to test

Full_ledclr:
	in r16, PINA 
	swap r16
	andi r16, $0F
	ldi ZL,LOW(led_table *2)
	ldi ZH,HIGH(led_table *2)
	ldi r18, 9
color:
	lpm r17, Z+
	mov r19, r17
	andi r19, $0F
	cp r19, r16
	breq LED
	dec r18
	brne color
LED:
	swap r17
	andi r17, $07
	
	sbrs r17, 2
	rjmp no_RLEDset
	sbi PORTD, 7
BLED:	
	sbrs r17, 1
	rjmp no_BLEDset
	sbi PORTC, 0
GLED:	
	sbrs r17, 0
	rjmp no_GLEDset
	sbi PORTC, 1

end:
ret

no_RLEDset:
cbi PORTD, 7
rjmp BLED
no_BLEDset:
cbi PORTC, 0
rjmp GLED
no_GLEDset:
cbi PORTC, 1
rjmp end

led_table: .db $00, $16, $22, $35, $48, $51, $6A, $74, $59
/*
push r14
   push r15
   push r16
   push r17
   push r18
   push r19
   push r20

   ldi ZH, HIGH(temperature)
   ldi ZL, LOW(temperature)
   ld r16, Z+
   ld r17,Z  


   ldi r19 , 0
   ldi r18, 0b01100100

   call div16u

   ldi ZH, HIGH(temperature_setting)
   ldi ZL, LOW(temperature_setting)
   ld r18, Z+
   ld r19, Z

   clz
   cp r16, r18
   breq temp_equal
   brne compare_temp1

compare2: 
   ldi ZH, HIGH(humidity)
   ldi ZL, LOW(humidity)
   ld r16, Z+
   ld r17,Z  


   ldi r19 , 0
   ldi r18, 0b01100100

   call div16u

   ldi ZH, HIGH(humidity_setting)
   ldi ZL, LOW(humidity_setting)
   ld r18, Z+
   ld r19, Z

   clz
   cp r16, r18
   breq hum_equal
   brne compare_hum

hum_equal:
	cbi PORTA, 4
	cbi PORTA, 5
	rjmp go_back
	

temp_equal:
	cbi PORTA, 6
	cbi portA, 7
	rjmp compare2

compare_temp1:
	cp r16, r18
	brlo turnpa7
	jmp turnpa6
turnpa7:
	sbi PORTA, 7
	cbi PORTA, 6
	jmp compare2
turnpa6:
	sbi PORTA, 6
	cbi PORTA, 7
	jmp compare2

compare_hum:
	cp r16, r18
	brlo turnpa5
	jmp turnpa4
turnpa5:
	sbi PORTA, 5
	cbi PORTA, 4
	jmp go_back
turnpa4:
	sbi PORTA, 4
	cbi PORTA, 5
	jmp go_back

go_back:
   pop r20
   pop r19
   pop r18
   pop r17
   pop r16
   pop r15
   pop r14*/

;***************************************************************************
;* 
;* "fsm" - Simplified Table Driven Finite State Machine
;*
;* Description:
;* This table driven FSM can handle 255 or fewer input symbols.
;*
;* Author:              Ken Short
;* Version:             2.0
;* Last updated:        11/09/15
;* Target:              ATmega16
;* Number of words:
;* Number of cycles:
;* Low regs modified:   r16, r18, r20, r21, r31, and r31
;* High registers used:
;*
;* Parameters:          present state in r25:r24 prior to call
;*                      input symbol in r16 prior to call
;*
;* Notes: 
;*
;***************************************************************************



;input symbols for example finite state machine

 ;input symbols equated to numerical values ;
.equ i0 = $80   ;down
.equ i1 = $81   ;up
.equ i2 = $82   ;set
.equ i3 = $83   ;humidity
.equ i4 = $84   ;temperature            
				
				;additional symbols would go here
.equ eol = $FF  ;end of list (subtable) do not change


;state table for example finite state machine
;each row consists of input symbol, next state address, task
;subroutine address


state_table:

display_temp:.dw i4, display_temp,  disp_meas_temp         ;state table for displaying
             .dw i3, display_humid, disp_meas_hum          ;temperature
			 .dw i2, setup_temp,    disp_temp_setting
			 .dw eol,display_temp,  do_nothing


display_humid:.dw i4, display_temp,   disp_meas_temp     ;state table for displaying
              .dw i3, display_humid,  disp_meas_hum      ;humidity
		      .dw i2, setup_humid,    disp_hum_setting
		      .dw eol,display_humid,  do_nothing

setup_temp:.dw i2, display_temp, load_temp_setting
           .dw i1, setup_temp, incr_temp_setting          ;state table for setting up 
           .dw i0, setup_temp, decr_temp_setting       ;temperature
		   .dw eol,setup_temp,    do_nothing

setup_humid:.dw i2, display_humid, load_hum_setting
            .dw i1, setup_humid, incr_hum_setting      ;state table for setting up
            .dw i0, setup_humid, decr_hum_setting      ;humidity
			.dw eol, setup_humid,    do_nothing


fsm:
;load Z with a byte pointer to the subtable corresponding to the
;present state
    lds r17, pstate
	mov ZL, r17		;load Z pointer with pstate address * 2
    add ZL, ZL ;since Z will be used as a byte pointer with the lpm instr.
	lds r17, pstate+1
    mov ZH, r17
    adc ZH, ZH


;search subtable rows for input symbol match
search:
    lpm r18, Z ;get symbol from state table
    cp r18, r16 ;compare table entry with input symbol
    breq match


;check input symbol against eol
check_eol:
    cpi r18, eol ;compare low byte of table entry with eol
    breq match

nomatch:
    adiw ZL, $06 ;adjust Z to point to next row of state table
    rjmp search ;continue searching

;a match on input value to row input value has been found
;the next word in this row is the next state address
;the word following that is the task subroutine's address
match:
	;make preseent state equal to next state value in row
	;this accomplishes the stat transition
    adiw ZL, $02 ;point to low byte of state address
    lpm r17, Z+		;copy next state addr. from table to preseent stat
	sts pstate, r17
	lpm r17, Z+
	sts pstate+1, r17

	;execute the subroutine that accomplihes the task associated
	;with the transition
    lpm r20, Z+ ;get subroutine address from state table
    lpm r21, Z ;and put it in Z pointer
    mov ZL, r20
    mov ZH, r21
    icall ;Z pointer is now used as a word pointer
    ret



;***************************************************************************
;* 
;* "taskn" - Stub subroutines for testing
;*
;* Description:
;* These subroutines are the tasks for the simple table driven FSM example.
;* When a program is being developed, you should start with each of these
;* subroutines consisting of just a nop and a return. You can then simulate
;* the program and verify that the transitions defined by you transition
;* table and original state diagram take place in response to input
;* sequences.
;*
;* Author:    Frank Yee
;* 
;* Last updated:11/17/2015
;*
;* Number of words:
;* Number of cycles:
;* Low registers used:
;* High registers used:
;*
;* Parameters:
;*
;* Notes: 
;*
;***************************************************************************


disp_meas_temp:
call read_humidicon                  ;call subroutine read_humidicon 

call temp_display

ret 

disp_meas_hum:

call read_humidicon                  ;call subroutine read_humidicon 

call humid_display

ret

disp_temp_setting:

ldi ZL, LOW(temperature_setting)         ;initialize pointer to temperature_setting
ldi ZH, HIGH(temperature_setting)

ld r16, Z+
ld r17, Z

ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_setting
ldi ZH, HIGH(temperature_unsaved)

st Z+, r16
st Z,  r17

ldi r27, $54 
call display_setpoint


ret

disp_hum_setting:

ldi ZL, LOW(humidity_setting)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(humidity_setting)

ld r16, Z+
ld r17, Z

ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(humidity_unsaved)

  st Z+, r16
  st Z,  r17

ldi r27, $48 
call display_setpoint

ret

incr_temp_setting:

ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(temperature_unsaved)

ld r16, Z+
ld r17, Z
inc r16

ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(temperature_unsaved)

st Z+, r16
st Z, r17 

ldi r27, $54 
call display_setpoint

ret 

incr_hum_setting:

ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to humidity_unsaved
ldi ZH, HIGH(humidity_unsaved)

ld r16, Z+
ld r17, Z
inc r16

ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to humidity_unsaved
ldi ZH, HIGH(humidity_unsaved)

st Z+, r16
st Z, r17 

ldi r27, $48 
call display_setpoint
ret 


decr_temp_setting:
ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(temperature_unsaved)

ld r16, Z+
ld r17, Z
dec r16

ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(temperature_unsaved)

st Z+, r16
st Z, r17 

ldi r27, $54 
call display_setpoint
ret 

decr_hum_setting:
ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(humidity_unsaved)

ld r16, Z+
ld r17, Z
dec r16

ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(humidity_unsaved)

st Z+, r16
st Z, r17 

ldi r27, $48 
call display_setpoint

ret

load_temp_setting:

ldi ZL, LOW(temperature_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(temperature_unsaved)

ld r16, Z+
ld r17, Z

ldi ZL, LOW(temperature_setting)         ;initialize pointer to temperature_setting
ldi ZH, HIGH(temperature_setting)

st Z+, r16
st Z, r17

call disp_meas_temp
ret

load_hum_setting:

ldi ZL, LOW(humidity_unsaved)         ;initialize pointer to temperature_unsaved
ldi ZH, HIGH(humidity_unsaved)

ld r16, Z+
ld r17, Z

ldi ZL, LOW(humidity_setting)         ;initialize pointer to temperature_setting
ldi ZH, HIGH(humidity_setting)

st Z+, r16
st Z,  r17

call disp_meas_hum
ret


do_nothing:
nop
nop
ret

buzzer:
sbi PORTD, 0
rcall var_delay
cbi PORTD, 0 
ret

isrread:
		 push r16			  ;save SREG values
         in r16, SREG
         push r16
		
			;ldi r16, $00
			in r16, PINA			 ;if set read keycode from PINA
            lsr r16					;right shift r16
            andi r16, $07			;mask r16 for except for the rightmost 3 bits
            ori r16, $80			;set the bit 7 of r16
			sts button_value, r16	;store
			 
        pop r16
		out SREG, r16
		pop r16
		
       reti
        
;***************************************************************************
;*
;* "SPI_humidicon_config" - SPI Configuratin for HumidIcon
;*
;* Description:
;* This subroutine unselects the HumidIcon and configures it for operation
;* with an ATmega16A operated a 1 MHz. Pin PA0 of the ATmega16A is used to
;* select the HumidIcon
;* Author:
;* Version:
;* Last updated:
;* Target: ATmega16A @ 1MHz
;* Number of words:
;* Number of cycles:
;* Low registers modified: none
;* High registers modified: none
;*
;* Parameters: none
;*
;* Returns: ATmeg16A's SPI configured to communicate with HumidIcon, but
;* not selected.
;* Notes:
;*
;***************************************************************************

SPI_humidicon_config:

 push r17			  ;save SREG values
 in r17, SREG
 push r17

 
 ldi r17, (1<<SPE)|(1<<MSTR)              ;enable SPI, Master, CLK= fck/16
 out SPCR, r17

 sbi PORTA, 0 

 pop r17          
 out SREG,r17       ;pop SREG values
 pop r17
 ret 

;***************************************************************************
;*
;* "read_humidicon_byte" - Read Byte from HumidIcon
;*
;* Description:
;* This subroutine reads a single byte from the HumidIcon. To do so it
;* transmits the value 0xAA. While any value can be used for this purpose,
;* 0xAA is used because this pattern will be easy to observe on an
;* oscilloscope. The select input to the HumidIcon must be asserted before
;* this subroutine is called and unasserted after it returns. This subroutine
;* does not manipulate the select input of the HumidIcon.
;*
;* The subroutine does not return until the SPI transfer is completed. The
;* subroutine determines whether the SPI transfer is complete by polling the
;* appropriate SPI status flag.
;*
;* Author:
;* Version:
;* Last updated:
;* Target: ATmega16A @ 1MHz
;* Number of words:
;* Number of cycles:
;* Low registers modified: none
;* High registers modified: r16
;*
;* Parameters: none
;*
;* Returns:
;* r16 - byte read from the HumidIcon
;*
;* Notes:
;*
;***************************************************************************
read_humidicon_byte:

ldi r17, 0xAA                ;move 0xAA to R17
out SPDR, r17                ;start transmission of 0xAA

waitset:
sbis SPSR, SPIF              ;check if transmission is complete
rjmp waitset       
in r16, SPDR                 ;read receive data into r16

ret

;***************************************************************************
;*
;* "read_humidicon" - Reads Humidity and Temperature from HumidIcon
;*
;* Description:
;* This subroutine selects the Humidicon by asserting PA0. It then calls
;* read_humidicon_byte four times to read the temperature and humidity
;* info. Is assigns the values read to the memory locations humidicon_byte1,
;* humidicon_byte2, humidicon_byte3, and humidicon_byte4, respectively. The
;* subroutine then deselects the HumidIcon.
;*
;* The subroutine then extracts the fourteen bits corresponding to the humidity
;* information and stores them right justified in the memory word humidity_raw.
;* Next if extracts the fourteen bits corresponding to the temperature
;* info and stores them in the memory word temperature_raw. The subroutine
;* then returns
;*
;* Author:
;* Version:
;* Last updated:
;* Target: ATmega16A @ 1MHz
;* Number of words:
;* Number of cycles:
;* Low registers modified:none
;* High registers modified: none
;*
;* Parameters: none
;*
;* Returns:
;* In data memory:
;* humidicon_byte1 - most significant (first) byte read from HumidIcon
;* humidicon_byte2 - second byte read from HumidIcon
;* humidicon_byte3 - third byte read from HumidIcon
;* humidicon_byte4 - least significant (fourth) byte read from HumidIcon
;*
;* humidity_raw - word containing right justified 14 bit humidity.
;* temperature_raw - word containing right justified 14 bit temperature.
;*
;* Notes:
;*
;***************************************************************************
read_humidicon:
		;store counter 
		push r18
		in r18, SREG
		push r18 
		
		;Configu humidicon SPI by calling previous subroutine
		rcall SPI_humidicon_config
		
		;initialize Z-ptr to point to first location in humidicon_byte
		/*ldi r18, 4
		ldi ZH, HIGH(humidicon_byte)
		ldi ZL, LOW(humidicon_byte)
		*/
		;enable slave device - HumidIcon
		cbi PORTA, 0


		rcall read_humidicon_byte
		sts humidicon_byte_1, r16					;post-increment the pointer
		rcall read_humidicon_byte
		sts humidicon_byte_2, r16
		rcall read_humidicon_byte
		sts humidicon_byte_3, r16
		rcall read_humidicon_byte
		sts humidicon_byte_4, r16

		;set /SS of HumidIcon = 1 (Deselected)			
		sbi PORTA, 0
		;extracts the fourteen bits corresponding to humidity info
		lds r21, humidicon_byte_1
		lds r22, humidicon_byte_2
		andi r21, $3F
		
		sts humidicon_byte_1,r21

		ldi ZH, HIGH(humidity_raw)
		ldi ZL, LOW(humidity_raw) 
		
		std Z+0, r22
		std Z+1, r21

		lds r23, humidicon_byte_3
		lds r24, humidicon_byte_4
		lsr r23
		ror r24
		lsr r23
		ror r24
		andi r23, $3F
		sts humidicon_byte_3,r23
		ldi ZH, HIGH(temperature_raw)
		ldi ZL, LOW(temperature_raw) 
		
		std Z+0, r24
		std Z+1, r23


		rcall delay

		;restore reg push into stack
		pop r18
		out SREG, r18
		pop r18

		ret

;***************************************************************************
;*
;* "scale_rh" - Scale Relative Humidity
;*
;* Description:
;* Computes scaled relative humidity in units of 0.01% RH from the raw 14-bit
;* relative humidity value from the Humidicon.
;*
;* Author:
;* Version:
;* Last updated:
;* Target: ATmega16A @ 1MHz
;* Number of words:
;* Number of cycles:
;* Low registers modified:
;* High registers modified:
;*
;* Parameters:
;*
;* Returns:
;* In data memory:
;* humidity - word containing RH in units of 0.01%
;*
;* Notes:
;*
;***************************************************************************

scale_rh:
        push r18
		in r18, SREG
		push r18 
		push r19
		push r22
		push r23
		push r24
		push r25

ldi r18, $10                    ;let r18 equal low byte of 10000 
ldi r19, $27                    ;let r19 equal high byte of 10000 

call mpy16u                     ;call the 16bit-16bit multiplication subroutine
                                ;do the multiplication humidity*10000 
                     
ldi r22,$fe                     ;let r22 equal to the lowest byte of 16382
ldi r23,$3f                     ;let r23 equal to the 2nd lowest byte of 16382
ldi r24,$00                     ;let r24 equal to the 2nd highest byte of 16382
ldi r25,$00                     ;let r25 equal to the highest byte of 16382

call div32u                     ;call subroutine div32u
                                ;do the division (humidity*10000 )/16382

ldi ZL, LOW(humidity)           ;initialize pointer to humidity
ldi ZH, HIGH(humidity)

st Z+,r18
st Z, r19
       
	    pop r25
        pop r24
		pop r23
		pop r22
		pop r19
        pop r18
		out SREG, r18
		pop r18
		
		ret

;***************************************************************************
;*
;* "scale_temp" - Scale Temperature
;*
;* Description:
;* Computes scaled temperature in units of 0.01% degrees C from the raw
;* 14-bit relative humidity value from the Humidicon.
;*
;* Author:
;* Version:
;* Last updated:
;* Target: ATmega16A @ 1MHz
;* Number of words:
;* Number of cycles:
;* Low registers modified:
;* High registers modified:
;*
;* Parameters:
;*
;* Returns:
;* In data memory:
;* temperature - word containing temperature in units of 0.01 degrees C.
;*
;* Notes: 
;*
;***************************************************************************

scale_temp:

        push r18
		in r18, SREG
		push r18 
		push r19
		push r22
		push r23
		push r24
		push r25

ldi r18,$74                     ;let r18 to be loaded with low byte of 16500
ldi r19,$40                     ;let r19 to be loaded with high byte of 16500

call mpy16u                     ;do the multiplication temp Output * 16500

ldi r22,$fe                     ;let r22 equal to the  lowest byte of 16382
ldi r23,$3f                     ;let r23 equal to the 2nd lowest byte of 16382
ldi r24,$00                     ;let r24 equal to the 2nd highest byte of 16382
ldi r25,$00                     ;let r25 equal to the highest byte of 16382

call div32u                     ;do the division (temp Output * 16500)/16382

ldi r22,$a0                     ;let r22 equal to low byte of 4000
ldi r23,$0f                     ;let r23 equal to high byte of 4000

sub r18,r22                     ; do the subtraction between 
sbc r19,r23                     ;((temp Output * 16500)/16382) -4000

ldi ZL, LOW(temperature)        ;initialize pointer to temprature
ldi ZH, HIGH(temperature)

st Z+,r18
st Z, r19

        pop r25
        pop r24
		pop r23
		pop r22
		pop r19
        pop r18
		out SREG, r18
		pop r18
		ret 


;***************************************************************************
;*
;* "mpy16u" - 16x16 Bit Unsigned Multiplication
;*
;* This subroutine multiplies the two 16-bit register variables 
;* mp16uH:mp16uL and mc16uH:mc16uL.
;* The result is placed in m16u3:m16u2:m16u1:m16u0.
;*  
;* Number of words	:14 + return
;* Number of cycles	:153 + return
;* Low registers used	:None
;* High registers used  :7 (mp16uL,mp16uH,mc16uL/m16u0,mc16uH/m16u1,m16u2,
;*                          m16u3,mcnt16u)	
;*
;***************************************************************************

;***** Subroutine Register Variables

.def	mc16uL	=r16		;multiplicand low byte
.def	mc16uH	=r17		;multiplicand high byte
.def	mp16uL	=r18		;multiplier low byte
.def	mp16uH	=r19		;multiplier high byte
.def	m16u0	=r18		;result byte 0 (LSB)
.def	m16u1	=r19		;result byte 1
.def	m16u2	=r20		;result byte 2
.def	m16u3	=r21		;result byte 3 (MSB)
.def	mcnt16u	=r22		;loop counter

;***** Code

mpy16u:	clr	m16u3		;clear 2 highest bytes of result
	clr	m16u2
	ldi	mcnt16u,16	;init loop counter
	lsr	mp16uH
	ror	mp16uL

m16u_1:	brcc	noad8		;if bit 0 of multiplier set
	add	m16u2,mc16uL	;add multiplicand Low to byte 2 of res
	adc	m16u3,mc16uH	;add multiplicand high to byte 3 of res
noad8:	ror	m16u3		;shift right result byte 3
	ror	m16u2		;rotate right result byte 2
	ror	m16u1		;rotate result byte 1 and multiplier High
	ror	m16u0		;rotate result byte 0 and multiplier Low
	dec	mcnt16u		;decrement loop counter
	brne	m16u_1		;if not done, loop more
	ret


;***************************************************************************
;*
;* "div32u" - 32/32 Bit Unsigned Division
;*
;* Ken Short
;*
;* This subroutine divides the two 32-bit numbers 
;* "dd32u3:dd32u2:dd32u1:dd32u0" (dividend) and "dv32u3:dv32u2:dv32u3:dv32u2"
;* (divisor). 
;* The result is placed in "dres32u3:dres32u2:dres32u3:dres32u2" and the
;* remainder in "drem32u3:drem32u2:drem32u3:drem32u2".
;*  
;* Number of words	:
;* Number of cycles	:655/751 (Min/Max) ATmega16
;* #Low registers used	:2 (drem16uL,drem16uH)
;* #High registers used  :5 (dres16uL/dd16uL,dres16uH/dd16uH,dv16uL,dv16uH,
;*			    dcnt16u)
;* A $0000 divisor returns $FFFF
;*
;***************************************************************************

;***** Subroutine Register Variables

.def	drem32u0=r12    ;remainder
.def	drem32u1=r13
.def	drem32u2=r14
.def	drem32u3=r15

.def	dres32u0=r18    ;result (quotient)
.def	dres32u1=r19
.def	dres32u2=r20
.def	dres32u3=r21

.def	dd32u0	=r18    ;dividend
.def	dd32u1	=r19
.def	dd32u2	=r20
.def	dd32u3	=r21

.def	dv32u0	=r22    ;divisor
.def	dv32u1	=r23
.def	dv32u2	=r24
.def	dv32u3	=r25

.def	dcnt32u	=r17

;***** Code

div32u:
	clr	drem32u0	;clear remainder Low byte
    clr drem32u1
    clr drem32u2
	sub	drem32u3,drem32u3;clear remainder High byte and carry
	ldi	dcnt32u,33	;init loop counter
d32u_1:
	rol	dd32u0		;shift left dividend
	rol	dd32u1
	rol	dd32u2    
	rol	dd32u3
	dec	dcnt32u		;decrement counter
	brne	d32u_2		;if done
	ret			;    return
d32u_2:
	rol	drem32u0	;shift dividend into remainder
    rol	drem32u1
    rol	drem32u2
	rol	drem32u3

	sub	drem32u0,dv32u0	;remainder = remainder - divisor
    sbc	drem32u1,dv32u1
    sbc	drem32u2,dv32u2
	sbc	drem32u3,dv32u3	;
	brcc	d32u_3		;   branch if reult is pos or zero

	add	drem32u0,dv32u0	;    if result negative restore remainder
	adc	drem32u1,dv32u1
	adc	drem32u2,dv32u2
	adc	drem32u3,dv32u3
	clc			;    clear carry to be shifted into result
	rjmp	d32u_1		;else
d32u_3:	sec			;    set carry to be shifted into result
	rjmp	d32u_1

;***************************************************************************
;*
;* "bin2BCD16" - 16-bit Binary to BCD conversion
;*
;* This subroutine converts a 16-bit number (fbinH:fbinL) to a 5-digit
;* packed BCD number represented by 3 bytes (tBCD2:tBCD1:tBCD0).
;* MSD of the 5-digit number is placed in the lowermost nibble of tBCD2.
;*
;* Number of words	:25
;* Number of cycles	:751/768 (Min/Max)
;* Low registers used	:3 (tBCD0,tBCD1,tBCD2)
;* High registers used  :4(fbinL,fbinH,cnt16a,tmp16a)	
;* Pointers used	:Z
;*
;***************************************************************************

;***** Subroutine Register Variables

.equ	AtBCD0	=13		;address of tBCD0
.equ	AtBCD2	=15		;address of tBCD1

.def	tBCD0	=r13		;BCD value digits 1 and 0
.def	tBCD1	=r14		;BCD value digits 3 and 2
.def	tBCD2	=r15		;BCD value digit 4
.def	fbinL	=r16		;binary value Low byte
.def	fbinH	=r17		;binary value High byte
.def	cnt16a	=r18		;loop counter
.def	tmp16a	=r19		;temporary value

;***** Code

bin2BCD16:
	ldi	cnt16a,16	;Init loop counter	
	clr	tBCD2		;clear result (3 bytes)
	clr	tBCD1		
	clr	tBCD0		
	clr	ZH		;clear ZH (not needed for AT90Sxx0x)
bBCDx_1:lsl	fbinL		;shift input value
	rol	fbinH		;through all bytes
	rol	tBCD0		;
	rol	tBCD1
	rol	tBCD2
	dec	cnt16a		;decrement loop counter
	brne	bBCDx_2		;if counter not zero
	ret			;   return

bBCDx_2:ldi	r30,AtBCD2+1	;Z points to result MSB + 1
bBCDx_3:
	ld	tmp16a,-Z	;get (Z) with pre-decrement
;----------------------------------------------------------------
;For AT90Sxx0x, substitute the above line with:
;
;	dec	ZL
;	ld	tmp16a,Z
;
;----------------------------------------------------------------
	subi	tmp16a,-$03	;add 0x03
	sbrc	tmp16a,3	;if bit 3 not clear
	st	Z,tmp16a	;	store back
	ld	tmp16a,Z	;get (Z)
	subi	tmp16a,-$30	;add 0x30
	sbrc	tmp16a,7	;if bit 7 not clear
	st	Z,tmp16a	;	store back
	cpi	ZL,AtBCD0	;done all three?
	brne	bBCDx_3		;loop again if not
	rjmp	bBCDx_1		


;***************************************************************************
;* 
;* "Subroutine_name" - clr_dsp_buff
;*
;* Description:
;* Initializes 8 byte display buffer named dsp_buff with blanks (0x20)
;*
;* Author:
;* Version:
;* Last updated:        103011
;* Target:              ATmega16 @ 1 MHz
;* Number of words:
;* Number of cycles:
;* Low registers used:  none
;* High registers used: r25,r26, Z-ptr
;*
;* Parameters:
;*  assumes: 8 byte buffer dsp_buff in SRAM
;*  returns: buffer cleared
;*  calls:  none
;*  called by: main program and diagnostics
;*
;* Notes: 
;*
;***************************************************************************

clr_dsp_buff:
     ldi R25, 8               ; load  length of  buffer.
     ldi R26, ' '              ; load blank/space into R26.
     ldi ZH, HIGH(dsp_buff) ; Load ZH and ZL as a pointer to 1st
     ldi ZL, LOW(dsp_buff)  ; byte of buffer for line 1.
   
    ;set DDRAM address to 1st position of line.
store_bytes:
     st  Z+, R26       ; store ' ' into 1st/next buffer byte and
                       ; auto inc ptr to next location.
     dec  R25          ; 
     brne store_bytes  ; cont until r25=0, all bytes written.
     ret

;***************************************************************************
;* 
;* "Subroutine_name" - load_msg
;*
;* Description:
;* Loads a predefined string msg into a specified diplay buffer.          
;*
;* Author:
;* Version:
;* Last updated:        103011
;* Target:              ATmega16 @ 1 MHz
;* Number of words:
;* Number of cycles:
;* Low registers used:  
;* High registers used: r16, Y, Z
;*
;* Parameters:
;*  assumes: Z = offset of message to be loaded.
;*  returns: buffer loaded with message
;*  calls:  none
;*  called by: main program and diagnostics
;*
;* Notes: 
;* Message structure:
;*   label:  .db <text string/message>, <end of string>
;*
;* Message examples (also see Messages at the end of this file/module):
;*   msg_1: .db "First Message ", 0   ; loads msg into buff 1, eom=0
;*.
;*   a) The last number (zero) is an 'end of string' indicator.
;*   b) Y = ptr to disp_buffer
;*      Z = ptr to message (passed to subroutine)
;***************************************************************************

load_msg:
     ldi r25, 8                ; load  length of  buffer.
     ldi r26, ' '              ; load blank/space into R26.
	 ldi r24, $2e
	 ldi YH, HIGH(dsp_buff)    ; Load YH and YL as a pointer to 1st
     ldi YL, LOW(dsp_buff)     ; byte of dsp_buff (Note - assuming 
        
get_msg_byte:
             
     st Y+, r27                ; else, store next byte of msg in buffer.
decrement:
	 dec r25                   ; Check if counter is 7
	 cpi r25,7                 ; if 7 leave the bit 6 blank
	 breq blank
	 
	 cpi r25,6                 ; Check if counter is 6
	 breq bit5                 ;if 6 leave the bit 5 blank 
	 cpi r25,5                 ; Check if counter is 5
	 breq bit4                 ; if 5 go to bit4
	 cpi r25,4                 ; Check if counter is 4
	 breq bit3                 ; if 4 go to bit3
	 cpi r25,3                 ; Check if counter is 3
	 breq bit2                 ; if 3 go to bit2
	 cpi r25,2                 ; Check if counter is 2
	 breq bit1                 ; if 2 go to bit1
	 cpi r25,1                 ; Check if counter is 1
	 breq bit0                 ; if 2 go to bit0

rjmp get_msg_byte             ; jump back and continue...
	
	blank:
	st Y+, r26                ;store the blank character in buffer
	rjmp decrement
  
    bit5:
	cpi r22,48                ;compare with 0 to see if bit 4 is 0
    breq blank                ;if bit 5 is 0, branch and leave bit 4 blank
	st Y+,r22                 ;store value of bit5 in buffer
    rjmp decrement
    
	bit4:
    cpi r21,48                ;compare with 0 to see if bit 4 is 0
	breq blank                ;if bit 4 is 0, branch and leave bit 4 blank
	st Y+, r21                ;store value of bit4 in buffer
	rjmp decrement
	
	bit3:
	st Y+, r10                ;store value of bit3 in buffer
	rjmp decrement
	bit2:
	st Y+, r24                ;store value of bit2 in buffer
	rjmp decrement
	bit1:
	st Y+, r19                ;store value of bit1 in buffer
	rjmp decrement
	
	bit0:
	st Y, r18               ;store value of bit1 in buffer
	
msg_loaded:
     
	 ret

.include "lcd_dogm08_asm_driver_m16.inc"

;***************************************************************************
;* 
;* "Subroutine_name" -  hex2ASCII
;* This subroutine converts a single 4-bit unsigned binary
;* value to its hexASCII representation. When this subroutine is called, 
;* it expects the 4-bit binary
;* value to be rightmost nibble in r16. The subroutine returns the required 
;* ASCII code in r16.
;***************************************************************************

hex2ASCII:

push r18						;save SREG values
in r18, SREG
push r18

 
ldi ZH, high (hexASCII * 2)    ;set Z to point to start of table
ldi ZL, low  (hexASCII * 2)
ldi r18, $00                   ;add offset to Z pointer
add ZL, r16
adc ZH, r18
lpm r16, Z                     ;load byte from table pointed to by Z

pop r18          
out SREG,r18                   ;pop SREG values
pop r18

hexASCII: .db $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46

ret 


;Delay subroutine 
var_delay:
push r16				;save registers to stack
in r16, SREG
push r16
push r17

ldi r16, 10				;delay for 10ms

;delay for ATmega16 @ 1MHz = r16 * 0.1 ms  
outer_loop:
    ldi r17, 255			
inner_loop:
	dec r17
	brne inner_loop
	dec r16
	brne outer_loop

pop r17
pop r16
out SREG, r16
pop r16				;pop stack back to registers

ret

delay:
		;save/ store registers & SREG
		push r20
		push r19
		in r19, SREG
		push r19

		ldi r19, 13					;  13  outer loop control variable
outerloop:
		ldi r20, 255				;inner loop control variable
innerloop:
		dec r20
		brne innerloop
		dec r19
		brne outerloop

		;restore registers & SREG
		pop r19
		out SREG, r19
		pop r19
		pop r20

		ret


temp_display:

push r17			                 ;save SREG values
in r17, SREG
push r17
push r16 

ldi r27, $54                         ;pass R27 to Load msg loop when measure temperature
ldi ZL, LOW(temperature_raw)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(temperature_raw)

ld r16, Z+
ld r17, Z


call scale_temp                      ;call subroutine scale_temp 

ldi ZL, LOW(temperature)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(temperature)

ld r16,Z+
ld r17,Z

call bin2BCD16  

mov r18,r13                    
andi r18, $0f                  ;force ms nibble to 0 
mov r16,r18                    ;copy r18 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r18,r16                    ;move the result to r18

swap r13
mov r19,r13
andi r19, $0f                  ;force ms nibble to 0 
mov r16,r19                    ;copy r22 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r19,r16                  

mov r20,r14
andi r20, $0f                  ;force ms nibble to 0 
mov r16,r20                    ;copy r23 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10,r16                    ;move the result to r23

swap r14
mov r21,r14
andi r21, $0f                  ;force ms nibble to 0 
mov r16,r21                    ;copy r21 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21,r16                    ;move the result to r21

mov r16,r15
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r22, r16

ldi ZL, LOW(measurandt)        ;initialize pointer to temp measurand 
ldi ZH, HIGH(measurandt)

st Z+,r10
st Z, r21

rcall load_msg				   ;load message into buffer(s).
rcall update_lcd_dog

call var_delay                 ; call the delay subroutine


pop r16
pop r17          
out SREG,r17       ;pop SREG values
pop r17

ret 

humid_display:

push r17			                 ;save SREG values
in r17, SREG
push r17
push r16 

ldi r27, $48  
ldi ZL, LOW(humidity_raw)            ;initialize pointer to humidity_raw
ldi ZH, HIGH(humidity_raw)

ld r16, Z+                           ;load data store in Y+ to r16
ld r17, Z

call scale_rh

ldi ZL, LOW(humidity)              ;initialize pointer to temperature_raw
ldi ZH, HIGH(humidity)

ld r16,Z+
ld r17,Z

call bin2BCD16

mov r18,r13                    
andi r18, $0f                  ;force ms nibble to 0 
mov r16,r18                    ;copy r18 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r18,r16                    ;move the result to r18

swap r13
mov r19,r13
andi r19, $0f                  ;force ms nibble to 0 
mov r16,r19                  ;copy r22 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r19,r16                     ;move the result to r22

mov r20,r14
andi r20, $0f                  ;force ms nibble to 0 
mov r16,r20                    ;copy r23 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10,r16                    ;move the result to r23

swap r14
mov r21,r14
andi r21, $0f                  ;force ms nibble to 0 
mov r16,r21                    ;copy r21 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21,r16                    ;move the result to r21

mov r16,r15
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r22, r16

ldi ZL, LOW(measurandh)        ;initialize pointer to temp measurand 
ldi ZH, HIGH(measurandh)

st Z+,r10
st Z, r21

load:
rcall load_msg				   ;load message into buffer(s).
rcall update_lcd_dog

call var_delay                 ; call the delay subroutine

pop r16
pop r17          
out SREG,r17       ;pop SREG values
pop r17

ret 


display_setpoint:


call bin2BCD16  

mov r16, r13
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10, r16

swap r13
mov r16, r13
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21, r16

mov r16,r14
andi r16,$0f
call hex2ASCII 
mov r22,r16

ldi r19, $30
ldi r18, $30

call load_msg				   ;load message into buffer(s).
call update_lcd_dog
call var_delay                 ; call the delay subroutine

ret


humid_measure: 


push r17			                 ;save SREG values
in r17, SREG
push r17
push r16 

ldi r27, $48  
ldi ZL, LOW(humidity_raw)            ;initialize pointer to humidity_raw
ldi ZH, HIGH(humidity_raw)

ld r16, Z+                           ;load data store in Y+ to r16
ld r17, Z

call scale_rh

ldi ZL, LOW(humidity)              ;initialize pointer to temperature_raw
ldi ZH, HIGH(humidity)

ld r16,Z+
ld r17,Z

call bin2BCD16

mov r18,r13                    
andi r18, $0f                  ;force ms nibble to 0 
mov r16,r18                    ;copy r18 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r18,r16                    ;move the result to r18

swap r13
mov r19,r13
andi r19, $0f                  ;force ms nibble to 0 
mov r16,r19                  ;copy r22 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r19,r16                     ;move the result to r22

mov r20,r14
andi r20, $0f                  ;force ms nibble to 0 
mov r16,r20                    ;copy r23 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10,r16                    ;move the result to r23

swap r14
mov r21,r14
andi r21, $0f                  ;force ms nibble to 0 
mov r16,r21                    ;copy r21 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21,r16                    ;move the result to r21

mov r16,r15
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r22, r16

ldi ZL, LOW(measurandh)        ;initialize pointer to temp measurand 
ldi ZH, HIGH(measurandh)

st Z+,r10
st Z, r21

pop r16
pop r17          
out SREG,r17       ;pop SREG values
pop r17

ret 

temp_measure:

push r17			                 ;save SREG values
in r17, SREG
push r17
push r16 

ldi r27, $54                         ;pass R27 to Load msg loop when measure temperature
ldi ZL, LOW(temperature_raw)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(temperature_raw)

ld r16, Z+
ld r17, Z


call scale_temp                      ;call subroutine scale_temp 

ldi ZL, LOW(temperature)         ;initialize pointer to temperature_raw
ldi ZH, HIGH(temperature)

ld r16,Z+
ld r17,Z

call bin2BCD16  

mov r18,r13                    
andi r18, $0f                  ;force ms nibble to 0 
mov r16,r18                    ;copy r18 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r18,r16                    ;move the result to r18

swap r13
mov r19,r13
andi r19, $0f                  ;force ms nibble to 0 
mov r16,r19                    ;copy r22 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r19,r16                  

mov r20,r14
andi r20, $0f                  ;force ms nibble to 0 
mov r16,r20                    ;copy r23 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10,r16                    ;move the result to r23

swap r14
mov r21,r14
andi r21, $0f                  ;force ms nibble to 0 
mov r16,r21                    ;copy r21 to parameter r16
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21,r16                    ;move the result to r21

mov r16,r15
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r22, r16

ldi ZL, LOW(measurandt)        ;initialize pointer to temp measurand 
ldi ZH, HIGH(measurandt)

st Z+,r10
st Z, r21


pop r16
pop r17          
out SREG,r17       ;pop SREG values
pop r17

ret

 convert:

 call bin2BCD16  

mov r16, r13
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r10, r16

swap r13
mov r16, r13
andi r16, $0f 
call hex2ASCII                 ;call subroutine hex2ASCII
mov r21, r16

ret 
