        NAME    CS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    CS.ASM                                  ;
;                             Chip Select Function                           ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;	This file contains the initialization for the Peripheral Chip Selects 
;	on the 80188. 
;
; References: EHDEMO.asm by Glen George 
;
; Revision History:
;     10/31/15	Yuan Ma		initial revision 
;     11/1/15   Yuan Ma         updated comments 
;     11/5/15   Yuan Ma         updated comments 
;

; include files:
$INCLUDE(CS.inc) 	



CGROUP  GROUP   CODE



CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; InitCS
;
; Description:		This function initializes the Peripheral Chip Selects
;			on the 80188. 
;                 
; Operation:		This function writes the initial values to the PACS
;			and the MPCS registers. 
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


InitCS    PROC        NEAR
          PUBLIC      InitCS	
					

	MOV	    DX, PACSreg		;set up to write to PACS register
	MOV	    AX, PACSval		
	OUT	    DX, AL		;write PACSval to PACS 

	MOV	    DX, MPCSreg		;set up to write to MPCS register
	MOV	    AX, MPCSval
	OUT	    DX, AL		;write MPCSval to MPCS  

	RET				;done so return			


InitCS    ENDP



CODE    ENDS



        END       
