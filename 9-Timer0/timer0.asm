        NAME    TIMER0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    TIMER0                                  ;
;                               Timer Functions                              ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;       This file contains three timer functions for multiplexing of the LED 
;	displays and keypad: InitTimer0, InstallTimer0Handler, and 
;       Timer0EventHandler. The functions initialize the timer and event 
;       handlers to output to the LED display. 	
;
;
; Table of Contents:
;	InitTimer0: 	    This function initializes Timer0 to generate
;			    an interrupt once every millisecond. 	
;	Timer0EventHandler: This function handles the timer interrupts
;			    by outputting the next digit to the LED display
;                           and scans the keypad  
;	InstallTimer0Handler: This function installs the event handler
;			      for the timer0 interrupts
;
; References: EHDEMO.asm by Glen George 
;
; Revision History:
;     10/31/15	Yuan Ma     wrote functions and their functional 
;		            specifications 
;     11/1/15   Yuan Ma     updated comments 
;     12/12/15  Yuan Ma     updated for keypad 


;include files 
$INCLUDE(timer0.inc)             ;includes definitions for timer initialization
$INCLUDE(int.inc)                ;includes definitions for interrupts   


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; external function declarations:
EXTRN	DisplayMux:NEAR 	;Displays a digit to the LED display 
EXTRN   KeypadScan:NEAR         ;Scans and debounces keypad presses 


; InitTimer0
;
; Description:		This function initializes the 80188 Timer 0 to 
;			generate an interrupt once every millisecond. The 
;			timer controller is also initialized to allow the 
;			timer interrupts. 
;                 
; Operation:		The function writes the timer values to the registers 
;                       in the PCB so that the count is reset. The maximum timer 
;                       count is set up so that every time the timer counts up 
;                       to the maximum time, an interrupt in generated. The 
;                       interrupt controller is also initialized and a timer EOI 
;                       is sent to the interrupt controller.  
;
; Arguments:		None
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	None
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
; Registers Changed:    AX, DX
;
; Limitations:		None 
;
; Author: Yuan Ma 
; Last Modified: 11/1/15
;


InitTimer0      PROC        NEAR
                PUBLIC      InitTimer0
	
                                ;initialize Timer #0 for an interrupt
                                ;every millisecond 
 	MOV     DX, Tmr0Count	;initialize the count register to 0
	XOR     AX, AX		;AX is cleared so first interrupt 
                                ;will occur a fixed amount of time 
                                ;after the control register is setup.
	OUT     DX, AL			

	MOV     DX, Tmr0MaxCntA	;set up max count for milliseconds 
                                ;per segment
	MOV     AX, COUNT_PER_MS ;count so interrupt is generated
                                 ;every milliseconds 
	OUT     DX, AL
	
	MOV     DX, Tmr0Ctrl	;set up the control register, interrupts on
	MOV     AX, Tmr0CtrlVal	;Timer 0 runs continuously using
                                ;only Maximum Count A and generates
                                ;an interrupt every time the max
                                ;count is reached  
	OUT     DX, AL

                                ;initialize the interrupt controller 
                                ;for timers 
	MOV     DX, INTCtrlrCtrl ;set up the interrupt control register
	MOV     AX, INTCtrlrCVal ;enables timer interrupts in the 
                                 ;timer controller 
	OUT     DX, AL
	
	MOV     DX, INTCtrlrEOI	;send a timer EOI to clear out the
                                ;the controller 
	MOV     AX, TimerEOI
	OUT 	DX, AL

	RET			;done so return			


InitTimer0	ENDP


; Timer0EventHandler 
;
; Description:		This function is the event handler for the Timer 0
;			interrupts. It calls DisplayMux every interrupt 
;			which outputs the next digit segment pattern to 
;			the LED display and KeyPadScan to check if a key has
;                       been pressed.    
;
; Operation:		This function calls DisplayMux and KeypadScan every interrupt 
;			so that a digit can be outputted and the keypad is 
;                       scanned and then sends an end of interrupt. The 
;                       registers are all saved.  
;
; Arguments:		None
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	None
;
; Global Variables:	None
;
; Input:		A 4x4 keypad 
;
; Output:		Displays a digit segment pattern on the LED 
;			display through the DisplayMux function 
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    None    
;
; Limitations:		None
;
; Author: Yuan Ma
; Last Modified: 10/31/15
;
;


Timer0EventHandler      PROC        NEAR
                    	PUBLIC      Timer0EventHandler


	PUSHA			    ;saves the registers since event 
                                    ;handlers should never change any register
                                    ;values 
	CALL    DisplayMux	    ;call function DisplayMux to mux display 
                                    ;for a digit
        CALL    KeypadScan          ;call function KeypadScan so we can scan 
                                    ;the keypad and debounce keys 

EndTimer0EventHandler:		    ;done taking care of the timer 
	MOV     DX, INTCtrlrEOI	    ;send an EOI to the interrupt controller 
	MOV     AX, TimerEOI
	OUT	DX, AL

	POPA			    ;restores the registers 		
	IRET 			    ;Event handlers end with IRET 
	

Timer0EventHandler 	ENDP



; InstallTimer0Handler
;
; Description:		Install the event handler for the Timer 0 interrupt
;
; Operation:		Writes the address of the Timer 0 event handler to 
;			the appropriate interrupt vector. 		
;
; Arguments:		None	
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	None
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
; Registers Changed:    flags, AX, ES 
;
; Limitations:		None 
;
; Author: Yuan Ma 
; Last Modified: 10/31/15
;

InstallTimer0Handler        PROC        NEAR
                            PUBLIC      InstallTimer0Handler

	
	XOR     AX, AX		;AX is cleared 
	MOV     ES, AX		;We move the cleared value to ES so that ES is now 
                                ;cleared (interrupt vectors are in segment 0)
				
                                ;stores the vector 
	MOV	ES:WORD PTR(DOUBLE_WORD_SIZE * Tmr0Vec), OFFSET(Timer0EventHandler)
	MOV	ES:WORD PTR(DOUBLE_WORD_SIZE * Tmr0Vec + WORD_SIZE), SEG(Timer0EventHandler) 

 	RET 			;all done, return 

InstallTimer0Handler		ENDP




CODE    ENDS



        END       
