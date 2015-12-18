;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  PARSER.INC                                ;
;                          Include for Serial Functions                      ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the constants used for the serial parsing functions.
;      This file also includes the states for the Mealy state machine and
;      the tokens.    
;
; Revision History:
;     11/27/15	Yuan Ma	    initial revision
;     11/29/15  Yuan Ma     added a few constants and comments 
;     12/12/15  Yuan Ma     added a few constants 
;
;
; Constant definitions for serial parsing functions:

NO_ERROR		EQU	0       ;indicates there were no parsing errors 
PARSE_ERROR		EQU	1       ;indicates there was a parsing error 
POSITIVE		EQU	+1      ;indicates parsed command is positive  
NEGATIVE	        EQU	-1      ;indicates parsed command is negative 
LASER_ON    	        EQU	1       ;value to pass into function SetLaser
                                        ;to turn the laser on 
POSITIVE_NUMBER_BOUND   EQU     32767   ;upper bound of Num (unsigned)  
NEGATIVE_NUMBER_BOUND   EQU     32768   ;lower bound of Num (unsigned) 
CIRCLE_DEGREES          EQU     360     ;number of degrees in a circle, ensures
                                        ;that large angles will remain the same
                                        ;angle, but be within correct bounds 
MAX_ELEVATION_ANGLE     EQU     60      ;bounds of elevation angle for 
                                        ;SetTurretElevation function (assumes 
                                        ;bounds are symmetrical) 
PARSE_LASER             EQU     1       ;value to indicate successful parsing of 
                                        ;laser command 
PARSE_DIRECTION         EQU     2       ;value to indicate successful parsing of 
                                        ;direction command 
PARSE_SPEED             EQU     3       ;value to indicate successfuly parsing of 
                                        ;speed command 

; Token constants:  
TOKEN_MASK	        EQU	01111111B ;mask high bit of token
NUM_TOKEN_TYPES	        EQU	12	  ;number of token types 


; Tokens
TOKEN_ABS_SPD		EQU	0	;token indicates set absolute speed: S or s 
TOKEN_REL_SPD 		EQU	1	;token indicates set relative speed: V or v
TOKEN_DIRECTION		EQU	2	;token indicates set direction: D or d 
TOKEN_TURR_ELE		EQU	3	;token indicates set turret elevation angle: E or e
TOKEN_TURR_ROT 		EQU	4	;token indicates rotate turret angle: T or t
TOKEN_LASER_FIRE	EQU	5	;token indicates turn laser on: F or f 
TOKEN_LASER_OFF		EQU	6	;token indicates turn laser off: O or o 
TOKEN_DIGIT		EQU	7	;token is a digit: 0 to 9
TOKEN_SIGN		EQU	8	;token is a sign + or - 
TOKEN_IGNORE 		EQU	9	;token is horizontal white space: spaces and tabs
TOKEN_EOS		EQU	10	;token is end of command: carriage return 
TOKEN_OTHER		EQU	11	;anything else

; States
ST_INITIAL		EQU	0   ;initial state 
ST_ERROR		EQU	1   ;error state 
ABS_SPEED		EQU	2   ;have decided that we need to set the absolute speed
ABS_SPEED_DIGIT		EQU	3   ;parsed a digit to set absolute speed 
ABS_SPEED_SIGN		EQU	4   ;parsed a sign to set absolute speed 
REL_SPEED		EQU	5   ;have decided that we need to set the relative speed 
REL_SPEED_DIGIT		EQU	6   ;parsed a digit to set relative speed 
REL_SPEED_SIGN		EQU	7   ;parsed a sign to set relative speed 
DIRECTION		EQU	8   ;have decided that we need to set the direction 
DIRECTION_DIGIT		EQU	9   ;parsed a digit to set the direction 
DIRECTION_SIGN		EQU	10  ;parsed a sign to set the direction 
TURR_ELE		EQU	11  ;have decided that we need to set the  
                                    ;turret elevation angle
TURR_ELE_DIGIT	        EQU	12  ;parsed a digit to set the turret elevation angle 
TURR_ELE_SIGN	        EQU	13  ;parsed a sign to set the turret elevation angle 
TURR_ROT		EQU	14  ;have decided we need to rotate the turret 
TURR_ABS_ROT_DIGIT	EQU	15  ;parsed a digit to rotate the absolute turret angle 
TURR_REL_ROT_SIGN       EQU     16  ;parsed a sign to rotate the relative turret angle 
TURR_REL_ROT_DIGIT	EQU	17  ;parsed a sign to rotate the relative turret angle 
ST_LASER_ON		EQU	18  ;have decided to turn laser on 
ST_LASER_OFF		EQU	19  ;have decided to turn laser off 
 
