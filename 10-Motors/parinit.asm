        NAME    ParInit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 ParInit.ASM                                ;
;                     Parallel Chip Initialization Function                  ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; File Description:
;	This file contains the initialization for the Peripheral Chip 
;	on the 80188. InitParallel initializes parallel port B to output in mode 0. 
;
;
; Revision History: 
;   11/14/15    Yuan Ma     ;initial revision 


; include files:
$INCLUDE(parinit.inc) 


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; InitParallel 
;
; Description:		This function initializes the parallel port B to output in 
;                       mode 0 by writing values to the motor control register.  
;                 
; Operation:		This function writes the address and value of the parallel
;                       control. 
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
; Last Modified: 11/14/15
;


InitParallel        PROC        NEAR
                    PUBLIC      InitParallel 	
					

	MOV     DX, MOTOR_CTRL_ADDRESS  ;write values to register to set up 
	MOV     AL, MOTOR_CTRL_VALUE 	;parallel port B 
	OUT     DX, AL			 

	RET				;done so return			


InitParallel		ENDP



CODE    ENDS



        END       
