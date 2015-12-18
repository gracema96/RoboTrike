        NAME    DISPLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    DISPLAY                                 ;
;                              Display Functions                             ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains five display functions for the RoboTrike and their
;      functional specifications: InitDisplay, Display, DisplayNum,
;      DisplayHex, and DisplayMux. The functions multiplex the LED display 
;      under interrupt control and displays an ASCII string, decimal number,
;      or hexadecimal number to the LED displays. 
;	
;
; Table of Contents:
;	Display:        This function is passed a <null> terminated string
;			to output to the LED display
;	DisplayNum:	This function outputs a 16-bit signed value in 
;			decimal to the LED display. 
;	DisplayHex:	This function outputs a 16-bit unsigned value in 
;			hexadecimal to the LED display. 
;	InitDisplay:	This function initializes the shared variables and
;			clears the buffers in preparation for displaying the 
;		        next digit
;	DisplayMux: 	This function multiplexes the LED under interrupt 
;			control by calling from the timer event handler 
;
;
; Revision History:
;     10/26/15  Yuan Ma     wrote outline
;     10/31/15	Yuan Ma	    wrote code for the functions and updated 
;			    functional specifications 
;     12/12/15  Yuan Ma     small commenting fixes 


;include files 
$INCLUDE(display.inc)             ;includes definitions for display constants
$INCLUDE(general.inc)             ;includes ASCII_NULL


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP


; external function declarations
	EXTRN	ASCIISegTable:BYTE 	;table of patterns for each ASCII
                                        ;character for 7-segment displays
	EXTRN	Dec2String:NEAR 	;converts a 16-bit signed binary 
                                        ;value to its decimal representation
                                        ;in a string 
	EXTRN	Hex2String:NEAR		;converts a 16-bit unsigned binary
                                        ;value to its hexadecimal 
                                        ;representation in a string 
		

; Display 
;
; Description:		This function is passed a <null> terminated string 
;			(str) to output to the LED display by writing to the 
;			display buffer. The length of the string that can be
;			displayed cannot be greater than MAX_DIGITS, which is 
;		        defined in the include file. If the string is 
;			longer, it will only display the string up to the 
;			maximum size. If the string is less than the maximum 
;			size, the string will be padded with blanks on the 
;                       left up until  the maximum size. The LEDs are configured
;                       for seven segment displays. 
;                 
; Operation:		The function loops through the digits in the passed
;			in string and checks the length of the string while
;		        also looking up the segment pattern of each digit
;			in the ASCII segment table. It will store the 
;			segment patterns in the segment buffer. For strings 
;			longer than MAX_DIGITS, it will cap the string up to 
;			MAX_DIGITS and for strings less than MAX_DIGITS, it 
;			will pad the string with blanks until the string
;			is MAX_DIGITS long.  
;
; Arguments:		str(ES:SI) - the address of the string to be displayed 
;				     which is passed in by reference 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	SegBuffer -  the segment buffer where the segment
;				     display patterns are stored (read and write) 
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    flags, AX, BX, DX, SI
;
; Limitations:		The number of digits able to be displayed is limited
;			to the maximum size.  	
;
; Author: Yuan Ma 
; Last Modified: 10/31/15
;


Display	        PROC        NEAR
                PUBLIC      Display

DisplayInit:
	MOV     DI, FIRST_DIGIT		;start out with the first digit to display
	;JMP	DisplayLoop	

DisplayLoop:			        ;start looping through string to determine if it 
                                        ;is too big (we need to cut off the string) or 
                                        ;too small (pad the display with blanks)
	CMP     DI, MAX_DIGITS	        ;check if the current digit is less than the 
                                        ;the maximum number allowed 
	JE      DisplayDone 	        ;if they are equal, then we have the maximum
                                        ;number of digits and we need to cut off the 
                                        ;display
	CMP     BYTE PTR ES:[SI], ASCII_NULL	;check if we have reached
                                                ;the end of the string 
	JE      PadWithBlanks	        ;if we have, then we need to pad the rest of 
                                        ;the display with blanks 

DigitPattern:			        ;get the LED pattern for each digit
	MOV     BX, OFFSET(ASCIISegTable)	;Get the ASCII table ready to look 
                                                ;up the segment patterns 	
	MOV     AL, ES:[SI]	        ;get the digit we want to look up 
	XLAT    CS:ASCIISegTable	;look up the digit in the table 
	LEA     BX, SegBuffer	        ;get the buffer where we want to store
                                        ;the segment patterns 
	MOV     [BX+DI], AL 	        ;store the segment pattern in the buffer 
	INC 	SI		        ;increment to the next digit of the string
	INC 	DI 		        ;increment to the next digit of the display 
	JMP	DisplayLoop	        ;loop back to check the next digit 	

PadWithBlanks:
	LEA     BX, SegBuffer 	        ;get the buffer where we want to store the 
                                        ;segment patterns 
	MOV 	BYTE PTR [BX+DI], BLANK_SPACE	;add a blank  
	INC 	DI 		        ;increment to the next digit of the display
	JMP     DisplayLoop	        ;loop back to check the next digit 

DisplayDone:			        ;we have the segment patterns for each digit
                                        ;of the string 
	RET				

Display		ENDP


; DisplayNum
;
; Description:		This function is passed a 16-bit signed value (n) to be 
;			outputted to the LED display in decimal. The decimal 
;			value is displayed as a 5 digit number padded with zeros
;			if necessary plus a leading sign digit to 
;                       indicate whether or not the number is positive or negative.       
;
; Operation:		This function calls the function Dec2String from
;			converts.asm to convert the argument n into its 
;			decimal representation. It stores the converted string
;			temporarily in StringBuffer and then passes the string to 
;			the display function to output. ES is also set to DS 
;                       since StringBuffer is in the data segment. 
;
; Arguments:		n (AX) - 16-bit signed value to convert to decimal and 
;				 display 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	StringBuffer - buffer to temporarily store the string
;				       from Dec2String before it is passed 
;				       into the Display function (write) 
;
; Global Variables:	None
;
; Input:		None
;
; Output:		None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    BX, DX, SI  
;
; Limitations:		None
;
; Author: Yuan Ma
; Last Modified: 10/31/15
;

DisplayNum      PROC        NEAR
                PUBLIC      DisplayNum

  PUSH    AX                    ;make sure AX is not changed 
  LEA     SI, StringBuffer	;moves the temporary place we want to 
                                ;store the string to the address 
  PUSH 	  SI			;we will lose SI when we call 
                                ;Dec2String, so we must save it first
  CALL	  Dec2String		;Call Dec2String to convert the 
                                ;binary number into a decimal string
  POP 	  SI			;we can get SI back now 
  MOV	  BX, ES		;we need to set ES equal to DS since
  MOV	  DS, BX		;the display function uses ES, but
                                ;we can’t directly move them because
                                ;the compiler won’t accept it so we
                                ;use BX temporarily 
  CALL   Display		;we call the display function to 
                                ;get the segment patterns of the 
                                ;converted string
  POP    AX                     ;restore AX 
  RET 

DisplayNum	ENDP


; DisplayHex
;
; Description:		This function is passed a 16-bit unsigned value (n) 
;			to be outputted in the LED display in hexadecimal. 
;			The hexadecimal number is displayed as a 4 digit 
;			number padded with zeros if necessary. 		
;
; Operation:		This function calls the function Hex2String from
;			converts.asm to convert the argument n into its 
;			hexadecimal representation. It stores the converted 
;			string temporarily in Stringbuffer and then passes the 
;			string to the display function to output. 
;
; Arguments:		n (AX) - 16-bit unsigned value to convert to hexadecimal 
;				 and display 	
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	StringBuffer - place to temporarily store the string
;				       from Hex2String before it is passed 
;				       into the display function (write) 
;
; Global Variables:	None
;
; Input:		None
;
; Output:		None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    BX, DX, SI
;
; Limitations:		None 
;
; Author: Yuan Ma 
; Last Modified: 10/31/15
;

DisplayHex      PROC        NEAR
                PUBLIC      DisplayHex
                
  PUSH    AX                    ;make sure AX is not changed  
  LEA	  SI, StringBuffer	;moves the temporary place we want to 
                                ;store the string to the address 
  PUSH 	  SI			;we will lose SI when we call 
                                ;Hex2String, so we must save it first
  CALL	  Hex2String		;Call Hex2String to convert the 
                                ;binary number into a hexadecimal string
  POP 	  SI			;we can get SI back now 
  MOV	  BX, ES		;we need to set ES equal to DS since
  MOV	  DS, BX		;the display function uses ES, but
                                ;we can’t directly move them because
                                ;the compiler won’t accept it so we
                                ;use BX temporarily 
  CALL   Display		;we call the display function to 
                                ;get the segment patterns of the 
                                ;converted string
  POP    AX                     ;restore AX                                
  RET 
  
DisplayHex	ENDP


; InitDisplay 
;
; Description:		This function initializes the display by clearing the 
;			display buffer and setting an initially empty
;			display by filling the display buffer with blanks. 		
;
; Operation:		The function creates an empty display by setting
;			all of the digits in the display buffer to be 
;			blank and resets the index. 
;
; Arguments:		None
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	SegBuffer - display buffer that will be cleared (write)
;			MuxCounter - current digit to display (write) 
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None
;
; Registers Changed:    flags, BX, ES, SI 
;
; Limitations:		None 	
;
; Author: Yuan Ma 
; Last Modified: 10/31/15
;


InitDisplay	PROC        NEAR
                PUBLIC      InitDisplay

IndexReset:				
	MOV     SI, FIRST_DIGIT		;initially sets the string
                                        ;position to the first digit
	MOV     MuxCounter, FIRST_DIGIT	;initially sets the first 
                                        ;digit to multiplex to the 
                                        ;first digit
	LEA     BX, SegBuffer		;get buffer that we wish to clear 

InitDisplayLoop:			;clears the buffer for segment display
	CMP     SI, MAX_DIGITS		;check if the current digit in the 
                                        ;string is greater than the maximum
                                        ;allowed in the display 
	JGE     InitDisplayDone		;if so, then we are done clearing the buffer 	 
	;JL     AddBlank 		;if not, we need to clear the buffer 
                                        ;by adding blanks

AddBlank:
	MOV     BYTE PTR [BX+SI], BLANK_SPACE 	;add a blank to the current digit 
	INC 	SI			;increment to the next digit of the 
                                        ;string
	JMP     InitDisplayLoop		;check the next digit of the string 


InitDisplayDone:			;we are done clearing the buffer 
	RET				

InitDisplay	ENDP


; DisplayMux
;
; Description:	        This function is called from the Timer0 event handler
;                       approximately every millisecond. It displays a digit in 
;		        SegBuffer to the LED display and starts over at 
;		        the beginning if it has reached a digit past the 
;		        last digit. 
;
; Operation:		The function first determines which digit of the 
;			display we want and if we are on the last digit, it
;			will loop around to the first digit again. Then it 
;			will look up the corresponding segment display
;			pattern previously found that is stored in the 
;			segment buffer and output this pattern on the LED.  
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	SegBuffer - buffer where the segment display patterns
;				    have been previously stored (write)
;			MuxCounter - current digit we want to display (write) 
;
; Global Variables:	None 
;
; Input:	        None
;
; Output:	        A digit on the LED display 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    flags, AX, BX, DX
;
; Limitations:		None 	
;
; Author: Yuan Ma 
; Last Modified: 10/31/15
;

DisplayMux      PROC        NEAR
                PUBLIC      DisplayMux

DisplayMuxInit:				
	MOV     DX, LED_ADDRESS		;get the address we want to display to
	ADD     DX, MuxCounter		;get the digit of the display we 
                                        ;want to write to 
	MOV     AX, MuxCounter		;get ready to look up the digit segment
                                        ;pattern from SegBuffer 

DisplayDigit:
	MOV     BX, OFFSET(SegBuffer)	;get the digit segment pattern buffer 
    
	XLAT 	SegBuffer		;look up the digit segment pattern by 
                                        ;treating the list as a byte table 
	OUT     DX, AL 			;output the segment pattern on the 
                                        ;display at the correct address 

CheckForWrap:				if we are on the last digit, we will
                                        ;need to loop around to the first digit  
	INC 	MuxCounter	        ;increment to get the next digit 
	CMP     MuxCounter, MAX_DIGITS	;check if the next digit is larger 
                                        ;than the maximum allowed
	JB      DisplayMuxDone		;if it is not larger, then we are done 
	;JA     DisplayMuxRestart	;if it is larger, we need to restart
                                        ;at the beginning

DisplayMuxRestart:	
	MOV     MuxCounter, FIRST_DIGIT ;the next digit was beyond
                                        ;the maximum so we have to 
                                        ;restart at the beginning 
                                      
DisplayMuxDone:				;we are done displaying a digit 
	RET				

DisplayMux	ENDP


CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'

StringBuffer   	DB	MAX_DIGITS DUP (?)  ;buffer that stores the string
                                            ;from Dec2String and Hex2String

SegBuffer	DB 	MAX_DIGITS DUP (?)  ;buffer to store the segment 
                                            ;display patterns 

MuxCounter      DB	?		    ;current digit to multiplex 


DATA    ENDS



        END       
