;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  SERIAL.INC                                ;
;                          Include for Serial Functions                      ;
;                                   EE/CS 51				     ;  
;				    Yuan Ma				     ;
;			       TA: Jennifer Lin			             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the constants used in the serial functions.  
;
; Revision History:
;     11/19/15	Yuan Ma	    initial revision
;     11/20/15  Yuan Ma     changed the value of a constant and added an 
;                           address 
;     11/22/15  Yuan Ma     added a few constants and finished commenting 
;

; Serial Register Addresses 
SERIAL_BASE	EQU	100H                ;base address for the serial port 
LCR_ADDRESS	EQU	SERIAL_BASE + 3H    ;line control register address 
IER_ADDRESS	EQU	SERIAL_BASE + 1H    ;interrrupt enable register address 
IIR_ADDRESS 	EQU     SERIAL_BASE + 2H    ;interrupt identification register 
					    ;address 
LSR_ADDRESS 	EQU     SERIAL_BASE + 5H    ;line status register address 
THR_ADDRESS 	EQU     SERIAL_BASE + 0H    ;transmitter holding register address 
                                            ;(write only) 
RBR_ADDRESS 	EQU      SERIAL_BASE + 0H   ;receiver buffer register address 
                                            ;(read only) 
MSR_ADDRESS 	EQU     SERIAL_BASE + 6H    ;modem status register address 
LSB_ADDRESS 	EQU     SERIAL_BASE + 0H    ;line status register address 

; Initialization values 
INIT_LCR_VAL	EQU	00000011B	;initial value to write to LCR 
                                	;0------- DLAB not set
                                	;-0------ No breaks set
                                	;--000--- No parities set
                                	;-----0-- 1 stop bit
                                	;------11 8 bit character word length 
INIT_IER_VAL	EQU	00001111B	;initial value to write to IER 
                                	;0000---- not used and always cleared
                                	;----1--- modem status interrupt set
                                	;-----1-- receiver line status interrupt set 
                                	;------1- transmitter holding register empty 
                                	;         interrupt set
                                	;-------1 received data available interrupt set
DEFAULT_BAUD_DIVISOR	EQU	11	;index of default baud rate in BaudRateTable 
DEFAULT_PARITY		EQU	0	;index of default parity in ParityTable

; Masks  
DLAB_MASK	        EQU 10000000B	;mask to set DLAB bit 
ERROR_MASK              EQU 00001110B   ;mask to only get the error bits, does
                                        ;not change the other bits 
PARITY_MASK             EQU 11000111B   ;mask to clear the parity bits,
                                        ;does not change other bits 
DISABLE_THRE_INT        EQU 11111101B   ;value to disable the THRE interrrupt 
                                        ;bit, does not change the other bits 
ENABLE_THRE_INT         EQU 00000010B   ;value to enable THRE interrupt bit, 
                                        ;does not change the other bits 
NO_INTERRUPTS           EQU 00000001B   ;value in IIR register if there are no 
                                        ;interrupts 

; Serial definitions: 
NO_KICKSTART_NEEDED     EQU 0       	;indicates kickstart is not needed 
KICKSTART_NEEDED        EQU 1       	;indicates kickstart is needed 
		
