;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 DISPLAY.INC                                ;
;                         Include for Display Functions                      ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the constants used in display.asm. 
;
; Revision History:
;     10/31/15	Yuan Ma     initial revision
;     12/12/15  Yuan Ma     deleted ASCII_NULL 
;
;
; Constant definitions for display functions: 

MAX_DIGITS 	EQU	8 	        ;there are 8 LED displays so at 
                                        ;most we can have 8 digits 
BLANK_SPACE	EQU	00000000B	;a blank space in ASCII 
FIRST_DIGIT	EQU	0 	        ;the first digit of the string or 
                                        ;display 
LED_ADDRESS	EQU	0000H	        ;the display address 
