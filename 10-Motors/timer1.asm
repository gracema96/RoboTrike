        NAME    TIMER1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    TIMER1                                  ;
;                               Timer1 Functions                             ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;       This file contains three timer functions for the RoboTrike motor 
;	functions: InitTimer1, InstallTimer1Handler, and Timer1EventHandler. 
;	The functions initialize the timer and event handlers to allow for 
;	pulse width modulation of the motors. 	
;
;
; Table of Contents:
;	InitTimer1: 		This function initializes Timer1 to generate
;				an interrupt once every (PWMCYCLETIME/SEVEN_BIT_RES)
;                               milliseconds 	
;	Timer1EventHandler:	This function handles the timer interrupts
;				by calling the function PWMCTEventHandler
;				to turn the motors on or off through pulse 
;				width modulation and turns the laser on or off  
;	InstallTimer1Handler:	This function installs the event handler
;				for the timer1 interrupts
;
;
; References: EHDEMO.asm by Glen George 
;
;
; Revision History:
;     11/13/15	Yuan Ma     wrote functions and their functional specifications 
;     11/14/15  Yuan Ma     updated comments 


;include files 
$INCLUDE(timer1.inc)             ;includes definitions for timer initialization 
$INCLUDE(int.inc)		 ;includes definitions for interrupts  


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; external function declarations:
EXTRN	PWMEventHandler:NEAR 	;called by the Timer1EventHandler
                                ;to implement pulse width 
                                ;modulation 


; InitTimer1
;
; Description:		This function initializes the 80188 Timer 1 to 
;			generate an interrupt once every (PWMCYCLETIME/SEVEN_BIT_RES)
;                       milliseconds. The timer controller is also initialized 
;		        to allow the timer interrupts. 
;                 
; Operation:		The function write the timer values to the registers
;			in the PCB so that the count is reset. The maximum timer
;			count is set up so that every time the timer counts up 
;			to the TMR1_MAX_COUNT, an interrupt in generated. The 
;		        interrupt controller is also initialized and a timer EOI
;			is sent to the interrupt controller. 
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
; Input:	        None
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
; Last Modified: 11/14/15
;


InitTimer1      PROC        NEAR
                PUBLIC      InitTimer1
	
                                ;initialize Timer 1 
 	MOV     DX, Tmr1Count	;initialize the count register to 0
	XOR     AX, AX		;AX is cleared so first interrupt 
                                ;will occur a fixed amount of time 
                                ;after the control register is setup.
	OUT     DX, AL			

	MOV     DX, Tmr1MaxCntA	;set up max count for milliseconds per segment 
	MOV     AX, TMR1_MAX_COUNT ;count so an interrupt is generated once
                                   ;every (PWMCYCLETIME/SEVEN_BIT_RES) milliseconds 
        OUT	    DX, AL
        
	MOV	DX, Tmr1Ctrl	;set up the control register, interrupts on 
	MOV     AX, Tmr1CtrlVal	;Timer 1 runs continuously using
                                ;only Maximum Count A and generates
                                ;an interrupt every time the max
                                ;count is reached  
	OUT     DX, AL

                                ;initialize the interrupt controller for timers 
	MOV     DX, INTCtrlrCtrl ;set up the interrupt control register
	MOV     AX, INTCtrlrVal	 ;enables timer interrupts in the 
                                ;timer controller 
	OUT     DX, AL
	
	MOV	DX, INTCtrlrEOI	;send a timer EOI to clear out the controller 
	MOV     AX, TimerEOI
	OUT 	DX, AL

	RET			;done so return			


InitTimer1	ENDP



; Timer1EventHandler 
;
; Description:		This function is the event handler for the Timer 1
;			interrupts. It calls PWMCTEventHandler every interrupt which
;			will implement pulse with modulation through the 
;			motors and turns the laser on or off.   
;
; Operation:		This function calls PWMCTEventHandler every interrupt 
;			so that the motors and laser can be turned on or off. 
;			 The registers are all saved.  
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
; Output:		The motors of the RoboTrike move and the laser is   
;			turned on/off through the PWMCTEventHandler function 
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
; Last Modified: 11/13/15
;
;


Timer1EventHandler      PROC        NEAR
                    	PUBLIC      Timer1EventHandler


	PUSHA			    ;saves the registers since event 
                                    ;handlers should never change any register
                                    ;values 
	CALL 	PWMEventHandler     ;call function PWMCTEventHandler
                                    ;to take care of pulse width modulation 
                                    ;of the motors and turn the laser on/off

EndTimer1EventHandler:		    ;done taking care of the timer 
	MOV     DX, INTCtrlrEOI	    ;send an EOI to the interrupt controller 
	MOV     AX, TimerEOI
	OUT     DX, AL

	POPA			    ;restores the registers 		
	IRET 			    ;Event handlers end with IRET 
	

Timer1EventHandler 	ENDP



; InstallTimer1Handler
;
; Description:		Install the event handler for the Timer 1 interrupt
;
; Operation:		Writes the address of the Timer 1 event handler to 
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
; Last Modified: 11/13/15
;
;

InstallTimer1Handler	PROC        NEAR
                        PUBLIC      InstallTimer1Handler

	
	XOR     AX, AX		;AX is cleared 
	MOV     ES, AX		;We move the cleared value to ES so 
                                ;that ES can be cleared (interrupt 
                                ;vectors are in segment 0)
				
                                ;stores the vector 
	MOV ES:WORD PTR(DOUBLE_WORD_SIZE * Tmr1Vec), OFFSET(Timer1EventHandler)
	MOV ES:WORD PTR(DOUBLE_WORD_SIZE * Tmr1Vec + WORD_SIZE), SEG(Timer1EventHandler) 

    RET 			    ;all done, return 


InstallTimer1Handler	ENDP




CODE    ENDS



        END       
