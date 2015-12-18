        NAME    INT2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   INT2.ASM                                 ;
;                                Int2 Functions                              ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; File Description: 
;	This files contains two functions for INT2 and their functional 
;	specifications: InitINT2 and InstallINT2Handler. These functions 
;	initialize the INT2 interrupt and installs the INT2 event handler 
;	to allow for the serial interrupts from the 16C450 serial port. 
;
;
; Table of Contents:  
;	InitINT2:		initializes INT2 interrupts 
;	InstallINT2Handler:	installs the event handler for the INT2 interrupts 
;
;
; Revision History:
;     11/19/15  Yuan Ma		wrote functions and their functional 
;				specifications 
;


;include files 
$INCLUDE(int2.inc) 	;includes definitions specific to INT2 
                        ;interrupt initialization 
$INCLUDE(int.inc)	;includes definitions for interrupts 


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        
;external function declarations
    EXTRN   SerialEventHandler:NEAR     ;handles the serial interrupts 

		

; InitINT2
;
; Description:		This functions initializes the 80188 interrupt 
;			controller to allow for INT2 interrupts. 
;
; Operation:		The interrupt controller is initialized by writing
;			the appropriate value to the INT2 control register. 
;			An INT2 EOI is sent at the end to deal with any 
;			pending interrupts. 
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
; Registers Changed:	AX, DX 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/19/15
;


InitINT2     	PROC        NEAR
                PUBLIC      InitINT2 

					;initialize the interrupt controller 
					;for INT2 interrupts 
	MOV	DX, INT2CtrlrCtrl	;set up the interrupt control register 
	MOV	AX, INT2CtrlrVal	;enables INT2 interrupts in the 
					;interrupt controller 
	OUT	DX, AL

	MOV	DX, INTCtrlrEOI		;send an INT2 EOI to clear out the controller 
	MOV	AX, INT2EOI		      
	OUT	DX, AL

	RET				;done so return 	

InitINT2	ENDP


; InstallINT2Handler 
;
; Description:		This function installs the event handler for
;			the INT2 interrupt	
;
; Operation:		Write the address of the INT2 event handler to 
;			the appropriate interrupt vector	
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
; Registers Changed:	flags, AX, ES  
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/19/15
;


InstallINT2Handler     	PROC        NEAR
                        PUBLIC      InstallINT2Handler 


	XOR	AX, AX 		;AX is cleared in case there is any value there 
	MOV	ES, AX		;we move the cleared value to ES so 
				;that ES can be cleared (interrupt
				;vectors are in segment 0

				;stores the vector
	MOV ES:WORD PTR(DOUBLE_WORD_SIZE * INT2Vec), OFFSET(SerialEventHandler)
	MOV ES:WORD PTR(DOUBLE_WORD_SIZE * INT2Vec + WORD_SIZE), SEG(SerialEventHandler) 

	RET			    ;all done, return 			

InstallINT2Handler	ENDP




CODE    ENDS




        END       
