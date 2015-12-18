;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  EVENTS.INC                                ;
;                             General Include File                           ;
;                                   EE/CS 51				     ;  
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the event type constants. 
;
; Revision History:
;     12/12/15	Yuan Ma		initial revision
;
; 
; Constants:

KEY_EVENT       EQU     01H     ;signifies a key event has occurred 
                                ;(a key has been pressed)  	   
RX_EVENT        EQU     02H     ;signifies a receive data event has occurred
                                ;(data has been received from the serial) 
ERROR_EVENT     EQU     03H     ;signifies an error event has occurred
                                ;(there is a serial error) 
