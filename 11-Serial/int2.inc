;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   INT2.INC                                 ;
;                          Include for INT2 Functions                        ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
; 	This file contains the the definitions used in the int2 file that are
;	necessary for INT2 interrupt initialization. 
;
; Revision History:
;     11/19/15	Yuan Ma		initial revision
;
;
; Definitions for INT2 functions: 
;
 
INT2CtrlrCtrl	EQU	0FF3CH	;address of interrupt control 
                                ;register for INT2
INT2CtrlrVal	EQU	00005H  ;value to write to INT2 Control Register
                                ;00000000000----- reserved
                                ;-----------0---- set edge triggering
                                ;------------0--- enable INT2 interrupts 
                                ;-------------101 set INT2 priority
INT2EOI		    EQU	0000EH	;INT2 EOI command 
INT2Vec		    EQU	14 	;interrupt vector for INT2 
