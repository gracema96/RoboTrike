;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               CONVERTS.INC                                 ;
;                     Include for Conversion Functions                       ;
;                                   EE/CS 51                                 ;
;                                    Yuan Ma                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File Description:
;      This file contains the constants used in converts.asm 
;
; Revision History:
;     10/17/15  Yuan Ma      Initial Revision 
;     12/6/15   Yuan Ma      deleted ASCII null value 
;
;DEFINITIONS: 
MAXPWR10               EQU             10000    ;the maximum power of 10 we 
                                                ;can have with 5 digits
MAXPWR16               EQU             4096     ;the maximum power of 16 we 
                                                ;can have with 4 digits 
SIGNCMP                EQU             8000h    ;checks the sign bit   
