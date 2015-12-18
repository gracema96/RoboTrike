;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 GENERAL.INC                                ;
;                             General Include File                           ;
;                                   EE/CS 51				     ;  
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains general constants used in multiple files   
;
; Revision History:
;     12/6/15	Yuan Ma		initial revision
;
; 
; Constants:

ASCII_NULL          EQU     00H     ;ASCII null value 
ASCII_EOS           EQU     13      ;carriage return value 
TABLE_WORD_INDEXED  EQU     1       ;ammount to shift table index value by since 
                                    ;table is indexed as words 
SET_FLAG    	    EQU	    1	    ;indicates flag is set
RESET_FLAG          EQU	    0 	    ;indicates isn’t set 
