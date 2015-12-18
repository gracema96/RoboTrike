;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  KEYPAD.INC                                ;
;                          Include for Keypad Functions                      ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the constants used in keypad.asm. 
;
; Revision History:
;     11/4/15	Yuan Ma     initial revision
;     12/5/15   Yuan Ma     changed KEY_EVENT
;
;
; Constant definitions for display functions: 

FIRST_ROW	    EQU	0	;represents the first row to be scanned
UNPRESSED_STATE	    EQU	0FH	;value for when no keys are pressed 
DEBOUNCE_TIME	    EQU	25 	;time to debounce for in milliseconds 
KEYPAD_ADDRESS	    EQU	80H	;keypad base address, represents the first row. 
                                ;Will be added to to get the other 3 rows 
KEY_MASK	    EQU	0FH     ;value to mask the top nibble since keys are 
                                ;only 4 bits longs 
NUM_ROWS	    EQU	4	;there are four rows in total 
AUTOREPEAT_RATE	    EQU	1000	;time in milliseconds that represents 
                                ;when a key press is registered as 
                                ;multiple key presses 
ROW_SHIFT_AMOUNT    EQU 4       ;number of bits we need to shift the 
                                ;row number by to get it in the high nibble
