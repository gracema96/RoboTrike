;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                              ;
;                                     INT.INC                                  ;
;                             Include for INT Functions                        ;
;                                    EE/CS 51                                  ;
;                                    Yuan Ma                                   ;
;                                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: 
;   This function contains the definitions used in int.asm that are necessary 
;   to initialize the illegal interrupt handler. 
;
; References: EHDEMO.inc by Glen George 
;
; Revision History:
;     10/30/15    Yuan Ma    initial revision 

; General Definitions

FIRST_RESERVED_VEC	EQU	1	;reserve vectors 1-3
LAST_RESERVED_VEC	EQU	3
NUM_IRQ_VECTORS         EQU     256     ;number of interrupt vectors
DOUBLE_WORD_SIZE        EQU     4       ;number of bytes in each entry of the 
                                        ;interrupt vector table 
WORD_SIZE               EQU     2       ;number of bytes in a word 
 
; Interrupt Controller Definitions

; Addresses
INTCtrlrEOI            EQU     0FF22H   ;address of interrupt controller EOI 
                                        ;register

; Register Values
NonSpecEOI             EQU     08000H   ;Non-specific EOI command
