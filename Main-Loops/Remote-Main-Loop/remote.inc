;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   REMOTE.INC                               ;
;                           Include for Remote Functions                     ;
;                                   EE/CS 51                                 ;
;                                    Yuan Ma 			             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File Description:
;      This file contains the constants used in the remote board functions. 
;
;
; Revision History:
;      12/4/15      Yuan Ma	    initial revision 
;      12/6/15      Yuan Ma     added some constants  
;
;
; Constant definitions for remote functions:

NO_EVENT_HANDLER	EQU 0	        ;no event handler, but needed for 
                            		;properly indexing table 
COMMAND_LEN         	EQU 8   	;length of a command 
DISPLAY_LEN         	EQU 8   	;number of characters that can be 
                                        ;displayed on LED display 
DISPLAY_BUF_LEN     	EQU DISPLAY_LEN + 1 ;number of characters that can be 
                                          ;stored in the display buffers
                                          ;same as the DISPLAY_LEN with an 
                                          ;extra character for the null terminator 
ERROR_SYMBOL        	EQU 'E'         ;character to display that indicates an error 
REMOTE_SYMBOL       	EQU 'r'         ;character to display that indicates serial 
