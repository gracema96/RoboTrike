;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  PARINIT.INC                               ;
;                       Include for Parallel Chip Select                     ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the definitions used in parinit initialization  
;
; Revision History:
;     11/14/15	Yuan Ma		initial revision
;
; Motor Control defintions: 

 MOTOR_CTRL_ADDRESS    EQU     183H     ;address of the parallel control 
                                        ;register 
 MOTOR_CTRL_VALUE      EQU     80H      ;value to write to parallel control
                                        ;register  
                                        ;1------- make mode set flag active 
                                        ;-00----- selects mode 0 for Group A 
                                        ;---0---- sets Port A to output 
                                        ;----0--- sets Port C (upper) to output 
                                        ;-----0-- selects mode 0 for Group B 
                                        ;------0- sets Port B to output 
                                        ;-------0 sets Port C (lower) to output 
