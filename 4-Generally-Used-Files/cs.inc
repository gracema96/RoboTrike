;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    CS.INC                                  ;
;                            Include for CS Function                         ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
; 	This file contains the the definitions used in CS.asm that are 
;	necessary to initialize the 80188 Peripheral Chip Selects. 
;
; References: EHDEMO.inc by Glen George 
;
; Revision History:
;     10/31/15	Yuan Ma		initial revision
;
;
; Definitions for Chip Select: 
;
; Addresses:
PACSreg		EQU	0FFA4H		;address of PACS register
MPCSreg		EQU	0FFA8H		;address of MPCS register 

; Control Register Values: 
PACSval		EQU	00003H		;PCS base at 0, 3 wait states
                            		;0000000000------ start at address 0
                            		;----------000--- reserved
                            		;-------------0-- wait for RDY inputs
                            		;--------------11 3 wait states
MPCSval		EQU	00183H		;PCS in I/O space, use PCS5/6, 3 wait states
                            		;0---------000--- reserved
                            		;-0000001-------- MCS is 8KB
                            		;--------1------- output PCS5/PCS6
                            		;---------0------ PCS in I/O space
                            		;-------------0-- wait for RDY inputs
                            		;--------------11 3 wait states 
