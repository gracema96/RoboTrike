        NAME    PARSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    PARSER                                  ;
;                           Serial Parsing Functions                         ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; File Description: 
;	This file contains the serial parsing functions for the RoboTrike and 
;	their functional specifications: InitParseSerial, ParseSerialChar,
;	ResetParseHandler, SetParseError, DoNothing, AddDigitAction, 
;	SetSignAction, LaserOnAction, LaserOffAction, SetAbsSpeedAction, 
;	SetRelSpeedAction, SetDirectionAction, SetTurrAngleAction, 
;	SetRelTurrAngleAction, SetTurrEleAction, and GetToken. 
;	Together, these functions initialize the serial parsing shared 
;	variables and parse the passed serial in data for the RoboTrike using
;	a Mealy state machine. 
;
;
; Table of Contents:
;	InitParseSerial:	initializes shared variables for serial parsing 
;   	(global) 
;	ResetParseHandler:	resets parser to parse new input 
;   	(local) 
;   	GetParseSuccessFlag: 	returns value of the ParseSuccessFlag 
;   	(global) 
;	ParseSerialChar:	parses the passed in input and executes 
;				any commands it receives and decodes 
;   	(local) 
;	SetParseError:		indicates if there was parsing error while parsing  
;   	(local)
;	DoNothing:		does nothing 
;   	(local)
;	AddDigitAction: 	adds a digit to the shared variable Num 
;   	(local)
;	SetSignAction: 		indicates whether a positive or negative sign has 
;                       	been parsed 
;   	(local)
;	LaserOnAction:		turns the laser on 
;   	(local)
;	LaserOffAction:		turns the laser off 
;   	(local)
; 	SetAbsSpeedAction:	RoboTrike is set to the absolute speed specified 
;                       	by the shared variable Num 
;   	(local)
;	SetRelSpeedAction:	RoboTrike is set to the relative speed specified 
;                       	by the shared variable Num 
;   	(local)
;	SetDirectionAction:	RoboTrike is set to move to the direction specified
;                       	by the shared variable Num relative to the current 
;                       	direction of movement 
;   	(local)
;	SetTurrAngleAction:	rotates the turret by the absolute angle specified
;                       	by the shared variable Num 
;   	(local)
;	SetRelTurrAngleAction:	rotates the turret by the relative angle specified
;                           	by the shared variable Num
;   	(local) 
;	SetTurrEleAction: 	laser on turret of the RoboTrike is set to be
;                       	elevated by the absolute angle specified by the 
;                       	shared variable Num 
;   	(local)
;	GetToken: 		gets an input token for the parser 
;   	(local) 
;
; Tables:
;	StateTable:         	state transition table for the Mealy state machine 
;	TokenTypeTable:     	table of token types 
;  	TokenValueTable:    	table of token values         
;
; Revision History:
;     11/23/15  Yuan Ma     wrote outline
;     11/27/15	Yuan Ma	    wrote functions and their functional 
;			    specifications and tables 
;     11/28/15  Yuan Ma     debugged code until passed all but 1 test 
;     11/29/15  Yuan Ma     updated comments 
;     12/12/15  Yuan Ma     added GetParseSuccessFlag function and updated
;                           some comments 
;


; local include files
$INCLUDE(parser.inc)        ;constants used for the serial parsing functions 
$INCLUDE(motors.inc)        ;constants necessary for motor functions  


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP


;external function declarations:
	EXTRN	SetMotorSpeed:NEAR      ;sets the speed and direction of the RoboTrike 
	EXTRN	SetLaser:NEAR           ;turns the laser on and off 
	EXTRN	SetTurretAngle:NEAR     ;sets absolute angle of the turret 
	EXTRN	SetRelTurretAngle:NEAR  ;sets relative angle of the turret 
	EXTRN	SetTurretElevation:NEAR ;sets relative elevation angle of the turret 
	EXTRN	GetMotorSpeed:NEAR      ;returns the current speed of the RoboTrike
                                        ;in AX 
	EXTRN	GetMotorDirection:NEAR  ;returns the current angle of the RoboTrike
                                        ;in degrees in AX 
		

; InitParseSerial  
;
; Description:		Initializes the shared variables for serial
;			parsing by resetting the state of the state
;			machine to the initial state, the sign of the 
;			argument to be positive, the argument that is used 
;			in the action functions as empty, and to indicate
;			that there are no errors. 	 
;
; Operation:		Sets the shared variable CurrentState to 
;			ST_INITIAL and calls the function ResetParseHandle
;			to reset the other shared variables. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	CurrentState - current state of the Mealy state 
;				       machine that the parser is at (written to) 
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
; Registers Changed:	None 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/27/15
;

InitParseSerial     	PROC        NEAR
                        PUBLIC      InitParseSerial 

	MOV	CurrentState, ST_INITIAL    ;initialize state machine to start 
                                            ;at the initial state 
	CALL	ResetParseHandler           ;call function ResetParseHandler
                                            ;to reset the other shared variables 
	RET				

InitParseSerial		ENDP


; ResetParseHandler  
;
; Description:		This function resets the shared variables for serial
;			parsing so the parser can start parsing new input. 
;                   	The sign of the argument is set to be positive as a
;                   	default, the argument that is used in the action 
;			action functions is initialized as empty, and there are
;                   	no errors. 
;
; Operation:		Sets the shared variable Sign to POSITIVE, the shared
;                   	variable Num to NO_NUM, and the shared variable ErrorFlag
;                   	to NO_ERROR. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	Sign      - indicates sign of the command  (written to) 
;                   	Num       - argument to be used in the action functions
;                                   (written to)  
;                   	ErrorFlag - indicates if there was error while parsing 
;                                   (written to) 
;
; Global Variables:	None 
;
; Input:	 	None
;
; Output:		None
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    None 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

ResetParseHandler     	PROC        NEAR
                        PUBLIC      ResetParseHandler

	MOV	Sign, POSITIVE      ;initialize the sign to be positive as 
                                    ;a default 
	MOV	Num, 0              ;there is initially no argument 
	MOV	ErrorFlag, NO_ERROR ;there are initially no parsing errors
	RET				

ResetParseHandler		ENDP


; GetParseSuccessFlag  
;
; Description:		This function returns the value of ParseSuccessFlag in AX.
;
; Operation:		Returns ParseSuccessFlag to AX. 
;
; Arguments:		None 
;
; Return Value:		AX - current value of ParseSuccessFlag  
;
; Local Variables:	None
;
; Shared Variables:	ParseSuccessFlag - indicates what type of command was 
;                                          parsed (1 for laser, 2 for motor 
;                                          direction and 3 for motor speed) 
;                                          (read) 
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
; Registers Changed:    AX
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 12/12/15
;
GetParseSuccessFlag     PROC        NEAR
                        PUBLIC      GetParseSuccessFlag
                        
    XOR     AX, AX 
    MOV     AL, ParseSuccessFlag
    RET 
    
GetParseSuccessFlag     ENDP

; ParseSerialChar
;
; Description:		This function is passed a character (c) from the 
;			the serial input in AL. The character is processed
;			as a serial command and returns the status of the
;			parsing operation in AX. A zero is returned if there
;			are no parsing errors due to the passed character
;			and PARSE_ERROR value is returned if there is a 
;			parsing error due to the passed in character.  		
;
; Operation:		The function will retrieve the token type and token 
;                   	value of the character to be parsed by calling the 
;                   	function GetToken. The token type will be returned in 
;                   	AH and the token value will be returned in AL, but 
;                   	these values will be stored in other registers so that 
;                   	AX can be used. The the transition entry in the State 
;                   	Table is calculated using NUM_TOKEN_TYPES, the CurrentState, 
;                   	and the token type and then converted to a table offset. 
;                   	The formula used for this step is:
;                   	Offset(CurrentState * NUM_TOKEN_TYPES + TokenType). Since
;                   	the State Table is organized in order of the current state
;                   	and the entries for each state are organized in order of
;                   	the token types, this will access the correct entry in the 
;                   	table. Next, the action associated with the transition 
;                   	is executed and then CurrentState is set to the next state
;                   	of the transition. After completing the transition, we 
;                   	must check if the transition resulted in a parsing error.
;                   	If there is a parsing error, we must return a nonzero 
;                   	value in AX, move the CurrentState to ST_INITIAL instead and
;                   	reset the parser to take care of overflow error. If there 
;                   	are no parsing errors, we simply return a zero value in AX. 
;
; Arguments:		c (AL) - ASCII character to be parsed; processed as a 
;				 serial command 
;
; Return Value:		AX - zero is returned if there are no parsing errors
;			     and a nonzero value is returned if there is a  
;			     parsing error  
;
; Local Variables:	None
;
; Shared Variables:	CurrentState -  current state of the Mealy state machine
;                                       that the parser is at (written to) 
;                   	ErrorFlag    -  indicates if there was an error while 
;                                       parsing (read from) 
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	If there were errors while parsing, a nonzero value is 
;                   	returned and the parser is reset and sent to the initial
;                   	state. 
;
; Algorithms:		Mealy State Machine 
;
; Data Structures: 	None 
;
; Registers Changed:    AX, BX, CX, DX, flags 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

ParseSerialChar      	PROC        NEAR
                        PUBLIC      ParseSerialChar
	

ParseSerialCharInit:
	CALL	GetToken	;call the function GetToken to get the token type
				;and value associated with the ASCII character
				;and return the token type in AH, token value in AL 
	MOV	DH, AH		;store the token type to free up AH 
	MOV	CH, AL		;store the token value to free up AX 
	;JMP	ComputeTransition ;next, we need to figure out what transition 
                                  ;to do 

ComputeTransition: 
	MOV	AX, NUM_TOKEN_TYPES ;find row in the State Table 
	MUL	CurrentState	;multiply by the CurrentState so that 
                                ;AX is now the start of the row of 
                                ;the CurrentState 
                                ;(CurrentState * NUM_TOKEN_TYPES )
	ADD	AL, DH		;get the actual transition
                                ;(CurrentState * NUM_TOKEN_TYPES + TokenType) 
	ADC	AH, 0 		;propagate low byte carry into high byte 
	IMUL	BX, AX, SIZE TRANSITION_ENTRY ;now convert to table offset 
	;JMP	DoAction        ;execute action of transition 

DoAction:
	MOV     ParseSuccessFlag, 0 ;for each action, reset the ParseSuccessFlag
	MOV	AL, CH		 ;get token value back, needed for the action 
	CALL	CS:StateTable[BX].ACTION ;do the action 
	;JMP	DoTransition     ;need to move to the next state 

DoTransition:
	MOV	CL, CS:StateTable[BX].NEXTSTATE ;set up CL with next state to 
                                                ;transition to 
	MOV	CurrentState, CL ;set CurrentState to this new state so now
                                 ;we have moved on to the next state 
	;JMP	CheckParseError  ;check if there were parsing errors while 
                                 ;executing the action 

CheckParseError:
	CM	ErrorFlag, PARSE_ERROR	;check if there are parsing errors
	JNE	NoParseError	;if not, we need to return a zero value 
	;JE	ParseError	;if so, we need to reset the parser and 
                                ;move into the initial state 

ParseError:
	MOV	AX, PARSE_ERROR ;return nonzero value to indicate 
                                ;there was a parsing error 
	MOV	CurrentState, ST_INITIAL  ;restart at the initial state  
	CALL	ResetParseHandler	  ;reset the parser 
	JMP	ParseSerialCharDone       ;done parsing char 

NoParseError:
	MOV	AX, NO_ERROR 	;return zero value to indicate no parsing 
                                ;errors 
	;JMP	ParseSerialCharDone     ;done parsing char 

ParseSerialCharDone:
	RET			;done, so return 

ParseSerialChar		ENDP


; SetParseError 
;
; Description:		This function sets the shared variable ErrorFlag to 
;                   	indicate there was a parsing error while executing one
;                   	of the action functions or when transitioning to the 
;                   	error state or when an illegal token has been processed 
;                   	(ex: TOKEN_EOS right after TOKEN_ELE; illegal because
;                   	no input argument was given after command that requires
;                   	input argument). 		
;
; Operation:		The function sets the shared variable ErrorFlag to 
;                   	PARSE_ERROR. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	ErrorFlag - indicates whether or not there was a parsing
;                                   error (written to) 
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
; Registers Changed:    None 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetParseError      	PROC        NEAR
                	PUBLIC      SetParseError 
	
	MOV	ErrorFlag, PARSE_ERROR	;set ErrorFlag to indicate there was an 
                                	;an error while parsing 
	RET				

SetParseError		ENDP


; DoNothing
;
; Description:		This function does nothing. Used when needed to transition 
;                   	to another state, but no action is required. 	  		
;
; Operation:		Does nothing 	
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
; Registers Changed:	None 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

DoNothing      	PROC        NEAR
                PUBLIC      DoNothing 

	NOP         ;does nothing 
	RET				

DoNothing	ENDP



; AddDigitAction 
;
; Description:		This function adds a digit to the shared variable Num
;                   	that will be passed into a command function. The token 
;                   	value is the digit that is added to the current Num
;                   	and the final Num represents the number that has been
;                  	parsed so far. Num is interpreted as an unsigned number 
;                   	and must be within NEGATIVE_NUMBER_BOUND and 
;                   	POSITIVE_NUMBER_BOUND. Used after receiving TOKEN_DIGIT 
;                   	while in a valid state to add a digit. 
;
; Operation:		This function stores the digit to be added to Num in BX
;                   	so that AX can be used for operations. The shared variable
;                   	Num is then multiplied by 10 to shift the digits left. If
;                   	there is overflow while multiplying, this indicates a 
;                   	parsing error and the function SetParseError is called. 
;                   	If there is no multiplication overflow, then the digit that
;                   	we want to add is added to the ones place. If there is 
;                   	overflow while adding, this indicates a parsing error
;                   	and the function SetParseError is called. If there is no 
;                   	addition error, we then check if the command we are parsing
;                   	is positive or negative. If the command is positive, we 
;                   	must make sure Num is below the POSITIVE_NUMBER_BOUND. If 
;                   	it is above, then this indicates a parsing error and the 
;                   	function SetParseError is called. If the command is negative, 
;                   	we must make sure Num is below the NEGATIVE_NUMBER_BOUND 
;                   	(since Num is unsigned). If it is above, then this 
;                   	indicates a parsing error and the function SetParseError is
;                   	called. If there are no parsing errors and we have 
;                   	successfully appended a digit, we store this new value as
;                   	Num. 
;
; Arguments:		AL - digit to add to shared variable Num 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	Num -  argument to be used in the action functions to 
;                              which a digit is added (read from and writte to)
;                   	Sign - indicates sign of the incoming command (read)
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	This function sets the ErrorFlag if any overflow occurs 
;                   	while multiplying or adding and if Num is not within 
;                   	NEGATIVE_NUMBER_BOUND and POSITIVE_NUMBER_BOUND. 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:	None 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

AddDigitAction      	PROC        NEAR
                        

	PUSHA               	;save registers 
AddDigitInit: 
	XOR	BX, BX		;clear BX in case anything is in BH since we will
				;be storing the token value in BX 
	MOV	BL, AL		;move digit to add to BL so we can use AX for other
				;operations 
	MOV	AX, Num 	;set up AX with the current Num 
	;JMP    ShiftDigits 	;now we need to shift the digits to the left 

ShiftDigits:
	MOV	CX, 10 		;multiply Num by 10 in order to shift all the digits
	MUL	CX		;left one place (Num = 10 * Num) 
	JC      AddDigitError   ;if there is overflow while multiplying, set 
				;the ErrorFlag 
	;JNC    AddNumDigit 	;otherwise, we can now add the digit to Num 

AddNumDigit:
	ADD	AX, BX		;add the digit to the ones place of Num 
				;(Num = 10 * Num + TokenValue) 
	JC      AddDigitError   ;if there is overflow while adding, set the 
				;ErrorFlag 
	;JNC    CheckDigitSign  ;otherwise, we must make sure the value is 
				;within the proper bounds 

CheckDigitSign:
	CMP     Sign, NEGATIVE      ;check if the command we are parsing is negative
	JE      CheckNegativeBound  ;if so, then we will check the lower bound  
	;JNE    CheckPositiveBound  ;if it is positive, then we will check the 
                                    ;upper bound 

CheckPositiveBound:
	CMP     AX, POSITIVE_NUMBER_BOUND   ;check to make sure the Num will be 
                                	    ;below the upper bound 
	JA      AddDigitError               ;if it is above the upper bound, set
                                            ;the ErrorFlag 
	JBE     StoreNum                    ;otherwise we have a valid number 

CheckNegativeBound:
	CMP     AX, NEGATIVE_NUMBER_BOUND   ;check to make sure the Num will be
                                	    ;above the lower bound (unsigned) 
	JA      AddDigitError               ;if it is above the lower bound, set 
                                            ;the ErrorFlag 
	;JBE     StoreNum                   ;otherwise we have a valid number 

StoreNum:
	MOV     Num, AX         ;store final number after adding digit to ones
                                ;places as the new Num 
	JMP     AddDigitDone    ;done adding a digit to Num 

AddDigitError:
	CALL	SetParseError	;call function SetParseError to indicate that
                                ;there was an error while parsing 
	;JMP    AddDigitDone        

AddDigitDone:
	POPA                    ;restore registers 
	RET			;done, so return 

AddDigitAction		ENDP




; SetSignAction 
;
; Description:		This function sets the shared variable Sign based on the 
;                   	passed in character in AL. This action is executed 
;                   	whenever a valid TOKEN_SIGN is encountered while parsing. 	
;
; Operation:		This function checks if the passed in character is 
;                   	POSITIVE. If so, the shared variable Sign is set to be
;                   	POSITIVE. If not, the shared variable Sign is set to be 
;                   	NEGATIVE. 
;
; Arguments:		AL - ASCII character that is currently being parsed; 
;                            either POSITIVE or NEGATIVE in this case 
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	Sign - indicates sign of the command (written to) 
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
; Registers Changed:    None 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetSignAction     	PROC        NEAR
                	PUBLIC      SetSignAction

CheckSign: 
	CMP	AL, POSITIVE	;check if the ASCII character currently
                                ;being parsed is a positive sign 
	JE	SetPositiveSign	;if so, indicate command will be positive
                                ;by setting shared variable Sign 
	;JNE	SetNegativeSign	;if not, indicate command will be negative 
                                ;be setting shared variable Sign 

SetNegativeSign:
	MOV	Sign, NEGATIVE	;indicate that the parsed ASCII character
                                ;was a negative sign and that the following
                                ;numerical command will be negative 
	JMP	SetSignDone 

SetPositiveSign:
	MOV	Sign, POSITIVE	;indicate that the parsed ASCII character
                                ;was a positive sign and that the following
                                ;numerical command will be positive 
	;JMP	SetSignDone

SetSignDone:  
	RET			;done setting the Sign, return 

SetSignAction		ENDP


; LaserOnAction
;
; Description:	        This function fires the laser by turning the laser on. 
;                       This action is used after receiving TOKEN_EOS in 
;                       ST_LASER_ON. 
;
; Operation:		This function sets AX (argument to be passed into SetLaser
;                       function_ as LASER_ON value and then calls
;                       the function SetLaser in order to turn the laser on. The
;                       parser is then reset so that it can start parsing new input.
;                       The ParseSuccessFlag is set to indicate successful laser 
;                       command parsing.  
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
; Registers Changed:    AX 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

LaserOnAction      	PROC        NEAR
                	PUBLIC      LaserOnAction 

	MOV     AX, LASER_ON        ;function SetLaser requires an argument in AX
                                    ;that indicates whether or not to turn the 
                                    ;laser on or off; we pass in a nonzero value
                                    ;in order to turn the laser on 
	CALL	SetLaser            ;call function SetLaser with a nonzero value to 
                                    ;turn the laser on 
	CALL	ResetParseHandler   ;reset parser so can start parsing new input 
        MOV     ParseSuccessFlag, PARSE_LASER  ;indicate we parsed a laser value  
	RET			    ;done turning the laser on, return     

LaserOnAction		ENDP



; LaserOffAction
;
; Description:		This function turns the laser off. This action is used
;                       after receiving TOKEN_EOS in ST_LASER_OFF.     		
;
; Operation:		This function sets AX (argument to be passed into SetLaser
;                       function) as LASER_OFF value and then calls the 
;                       function SetLaser in order to turn the laser off. The 
;                       parser is then reset so that it can start parsing new input.
;                       The ParseSuccessFlag is set to indicate successful laser 
;                       command parsing.   
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
; Registers Changed:    AX 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29 /15
;

LaserOffAction      	PROC        NEAR

	MOV     AX, LASER_OFF       ;function SetLaser requires an argument in AX
                                    ;that indicates whether or not to turn the 
                                    ;laser on or off; we pass in a zero value
                                    ;in order to turn the laser off 
	CALL	SetLaser            ;call function SetLaser with a zero value to 
                                    ;turn the laser off 
	CALL	ResetParseHandler   ;reset parser so can start parsing new input 
        MOV     ParseSuccessFlag, PARSE_LASER     ;indicate we parsed a laser value   
	RET			    ;done turning the laser off, return 

LaserOffAction		ENDP



; SetAbsSpeedAction
;
; Description:		This function sets the Robotrike to the absolute speed
;                       specified by the shared variable Num (will only accept
;                       non-negative values). Num is interpreted as an unsigned
;                       integer value that fits in 15-bits. A speed of zero
;                       would indicate halting the RoboTrike. Used after 
;                       receiving TOKEN_EOS in the ABS_SPEED_DIGIT state.   		
;
; Operation:	        This function first checks if the shared variable Sign
;                       indicates the command will be negative. If so, this is 
;                       a parsing error since only non-negative values are 
;                       accepted and the function SetParseError will be called
;                       to set the ErrorFlag. If Sign indicates the command is 
;                       positive, the command argument specified by Num will be 
;                       set as AX (speed argument to be passed into SetMotorSpeed)
;                       and the IGNORE_ANGLE will be set as BX (angle argument
;                       to be passed into SetMotorSpeed) to ensure that only the
;                       speed is changed. Finally the function SetMotorSpeed is 
;                       called to set the new speed and the parser is reset so 
;                       that it can start parsing new input. The ParseSuccessFlag 
;                       is set to indicate successful speed command parsing.  
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	Num -  argument to be used in SetMotorSpeed function (read)
;                       Sign - indicates sign of the incoming command (read) 
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	If a negative value is about to be used for absolute 
;                       speed, a parsing error will be indicated. 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    None 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetAbsSpeedAction     	PROC        NEAR

        PUSHA                           ;save registers 
CheckAbsSpeedSign:
	CMP     Sign, NEGATIVE		;check if incoming command is negative 
	JE      AbsSpeedError 		;if so, set ErrorFlag since we only accept
                                        ;non-negative values since this is the 
                                        ;absolute speed 
	;JNE	AbsSpeedSuccess         ;if not, then we can successfully set the speed 

AbsSpeedSuccess:
	MOV     AX, Num			;function SetMotorSpeed requires a speed 
                                        ;argument in AX; we will pass in the number
                                        ;we just finished parsing 
	MOV     BX, IGNORE_ANGLE        ;function SetMotorSpeed requires an angle 
                                        ;argument in BX; we will pass in IGNORE_ANGLE
                                        ;since we only want to change the speed 
	CALL	SetMotorSpeed		;call function SetMotorSpeed with new speed
                                        ;and IGNORE_ANGLE to set the absolute speed 
	CALL	ResetParseHandler	;reset parser so can start parsing new command
        MOV     ParseSuccessFlag, PARSE_SPEED ;indicate parsed a motor speed command  
	JMP     AbsSpeedDone            ;done setting the absolute speed 

AbsSpeedError:
	CALL	SetParseError		;call function SetParseError to indicate that
                                        ;there was an error while parsing 
	;JMP	AbsSpeedDone

AbsSpeedDone: 
        POPA                            ;restore register 
	RET				;done, so return 

SetAbsSpeedAction	ENDP



; SetRelSpeedAction
;
; Description:		This function accelerates or decelerates the RoboTrike 
;                       by the relative speed specified by the shared variable 
;                       Num and the shared variable Sign. A relative speed of
;                       REST_SPEED has no effect on the RoboTrike's speed. If
;                       after the relative speed to the current speed, the 
;                       resulting speed is negative, the resulting speed is then 
;                       manually set to REST_SPEED and the RoboTrike is halted.
;                       If after adding the relative speed to the current speed, 
;                       the resulting speed is over MAX_SPEED, the resulting 
;                       speed is then manually set to MAX_SPEED as the RoboTrike
;                       cannot move any faster. Used after receiving TOKEN_EOS in 
;                       the REL_SPEED_DIGIT state.  	
;
; Operation:		This function first obtains the relative speed in BX and 
;                       calls the function GetMotorSpeed to return the current
;                       motor speed in BX. The function will check if the relative
;                       speed is positive or negative. If the relative speed is 
;                       positive, the relative speed will be added to the current
;                       speed and the function will check if the resulting speed
;                       has exceeded MAX_SPEED. If the resulting speed exceeds 
;                       MAX_SPEED, the speed is then manually set to be the 
;                       MAX_SPEED. If the relative speed is negative, the 
;                       relative speed is subtracted from the current speed and the 
;                       function will check if this unsigned subtraction results
;                       in underflow (the resulting speed is negative). 
;                       If the resulting speed is negative, the speed is then 
;                       manually set to be the REST_SPEED. Finally the function 
;                       SetMotorSpeed is called to set the relative speed with 
;                       the resulting speed set as AX (speed argument to be 
;                       passed into SetMotorSpeed) and the IGNORE_ANGLE set as BX
;                       (angle argument to be passed into SetMotorSpeed) to 
;                       ensure that only the speed is changed. The parser is reset
;                       at the end so that it can start parsing new input by calling 
;                       ResetParseHandler. The ParseSuccessFlag is set to indicate 
;                       successful speed command parsing.  
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	Num -  argument to be used in SetMotorSpeed function (read)
;                       Sign - indicates sign of the incoming command (read)
;
; Global Variables:	None 
;
; Input:	        None
;
; Output:		None
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    None 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetRelSpeedAction     	PROC        NEAR

                        
        PUSHA                           ;save registers 
RelSpeedInit:
	MOV     BX, Num                 ;set up BX with the relative speed since 
                                        ;we will need AX for GetMotorSpeed 
	CALL	GetMotorSpeed		;call function GetMotorSpeed to return the 
                                        ;current motor speed in AX 
	;JMP	CheckRelSpeedSign       ;check if we need to accelerate or decelerate 

CheckRelSpeedSign:
	CMP     Sign, NEGATIVE		;check if the relative speed is negative 
	JE      SubtractSpeed		;if negative, we need to subtract the relative
                                        ;speed from the current speed to decelerate 
	;JNE	AddSpeed		;if positive, we need to add the relative speed
                                        ;to the current speed to accelerate
                                
AddSpeed:
	ADD 	AX, BX			;add the relative speed(BX) to the current
                                        ;speed(AX) to accelerate the RoboTrike 
	JC      SetSpeedMax		;if this addition carries, we need to manually
                                        ;set the speed to MAX_SPEED since it is over
                                        ;MAX_SPEED 
	;JNC	CheckRelSpeedMax	;if there is no overflow, check if the resulting
                                        ;speed is over MAX_SPEED just in case 

CheckRelSpeedMax:
	CMP     AX, MAX_SPEED           ;check if resulting speed is over MAX_SPEED 
	JBE     RelSpeedSuccess		;if not, we can now set the relative speed
	;JA     SetSpeedMax		;if it is, we need to manually set the speed 
    
SetSpeedMax:
	MOV     AX, MAX_SPEED		;manually set speed as MAX_SPEED 
	JMP     RelSpeedSuccess		;now we can set the relative speed 

SubtractSpeed:                  
        SUB     AX, BX                  ;subtract the relative speed(BX) from current
                                        ;speed(AX) to decelerate the RoboTrike 
        JNC     RelSpeedSuccess         ;if there is no underflow, we can set the 
                                        ;relative speed 
        ;JC      SetSpeedHalt           ;if this unsigned subtraction carries, we need 
                                        ;to manually set the speed to REST_SPEED since
                                        ;it is negative 

SetSpeedHalt:
	MOV     AX, REST_SPEED 		;manually set speed to REST_SPEED and halt
                                        ;RoboTrike 
	;JMP	RelSpeedSuccess         ;now we can set the relative speed 
	 
RelSpeedSuccess:
	MOV     BX, IGNORE_ANGLE        ;function SetMotorSpeed requires an angle 
                                        ;argument in BX; we will pass in IGNORE_ANGLE
                                        ;since we only want to change the speed 
	CALL	SetMotorSpeed           ;call function SetMotorSpeed with new speed
                                        ;and IGNORE_ANGLE to set the relative speed                           
        CALL    ResetParseHandler       ;reset parser so can start parsing new command
        MOV     ParseSuccessFlag, PARSE_SPEED ;indicate parsed a motor speed command 
	;JMP	RelSpeedDone            ;done setting relative speed 

RelSpeedDone:
        POPA                            ;restore registers 
	RET				;done, so return 

SetRelSpeedAction	ENDP



; SetDirectionAction
;
; Description:	        This function sets the RoboTrike to move in the direction
;                       relative to the current direction of movement specified 
;                       by the shared variable Num and the shared variable Sign. 
;                       Num is interpreted as a signed 16-bit integer value in 
;                       units of degrees. A positive angle indicates a direction
;                       to the right (looking down on the RoboTrike), while a 
;                       negative angle indicates a direction to the left. Before 
;                       the relative angle is added to the current angle, it is 
;                       adjusted to be within the NEGATIVE_NUMBER_BOUND and 
;                       POSITIVE_NUMBER_BOUND. Used after receiving TOKEN_EOS 
;                       in the DIRECTION_DIGIT state. 
;
; Operation:	        This function firsts stores the relative angle in BX and
;                       checks if the Sign shared variable indicates the angle 
;                       is positive or negative. If the angle is positive, we 
;                       subtract CIRCLE_DEGREES from the relative angle in order to
;                       make sure the resulting angle never exceeds
;                       POSITIVE_NUMBER_BOUND. It the angle is negative, we will
;                       get the negative relative angle and then add CIRCLE_DEGREES
;                       to the relative angle in order to make sure the 
;                       resulting angle never exceeds NEGATIVE_NUMBER_BOUND. 
;                       Once we have finished adjusting the angle, we can
;                       call the function GetMotorDirection to return the current 
;                       motor direction in AX and add this value to the relative
;                       angle. We then call the function SetMotorSpeed with 
;                       IGNORE_SPEED set as AX (speed argument to be passed into 
;                       SetMotorSpeed) to ensure that only the angle is changed
;                       and the resulting angle in BX (angle argument to be passed
;                       into SetMotorSpeed) to set the new RoboTrike direction. 
;                       The parser is reset at the end so that it can start 
;                       parsing new input. The ParseSuccessFlag is set to indicate 
;                       successful direction command parsing.  
;
; Arguments:	        None 
;
; Return Value:	        None 	 
;
; Local Variables:	None
;
; Shared Variables:	Num - argument to be used in SetMotorSpeed function (read) 
;                       Sign - indicates sign of the incoming command (read) 
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
; Registers Changed:    None 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetDirectionAction      PROC        NEAR
       
       
    PUSHA                       ;save registers 
SetDirectionInit:
    MOV     BX, Num             ;set up BX with the relative angle since this
                                ;since we need AX for GetMotorDirection 
    CMP     Sign, NEGATIVE      ;check if the relative angle is negative 
    JE      NegativeDirection   ;if negative, we need to convert to negative 
                                ;angle value 
    ;JNE    PositiveDirection   ;if positive, we keep as positive 
    
PositiveDirection:
    SUB     BX, CIRCLE_DEGREES  ;subtract CIRCLE_DEGREES from the positive 
                                ;relative angle to keep the same angle but
                                ;to make sure the resulting angle will be 
                                ;within POSITIVE_NUMER_BOUND 
    JMP     DirectionSuccess    ;can now set the relative direction 
    
NegativeDirection:
    NEG     BX                  ;negate the relative angle since the 
                                ;relative angle should be negative 
    ADD     BX, CIRCLE_DEGREES  ;add CIRCLE_DEGREES to the negative relative
                                ;angle to keep the same angle but to make 
                                ;sure the resulting angle will be within 
                                ;NEGATIVE_NUMBER_BOUND 
    ;JMP    DirectionSuccess    ;can now set the relative direction 

DirectionSuccess:
    CALL    GetMotorDirection   ;call function GetMotorDirection to return 
                                ;the current motor direction in AX
    ADD     BX, AX              ;add the current angle to the relative angle
                                ;to get the resulting angle in BX since 
                                ;SetMotorSpeed requires an angle argument in BX
    MOV	    AX, IGNORE_SPEED	;function SetMotorSpeed requires a speed
                                ;argument in AX; we will pass in IGNORE_SPEED
                                ;since we only want to change the angle 
    CALL    SetMotorSpeed	;call function SetMotorSpeed with IGNORE_SPEED
                                ;and the new angle to set the relative angle                         
    CALL    ResetParseHandler   ;reset parser so can start parsing new command
    MOV     ParseSuccessFlag, PARSE_DIRECTION ;indicate parsed a direction command   
    ;JMP    DirectionDone       ;done setting relative angle 

DirectionDone: 
    POPA                        ;restore registers 
    RET	                        ;done, so return 			

SetDirectionAction	ENDP



; SetTurrAngleAction
;
; Description:          This function rotates the turret by the absolute angle
;                       specified by the shared variable Num. Num is interpreted
;                       as an absolute (non-negative) angle in degrees with an 
;                       angle of zero indicating straight ahead relative to the
;                       RoboTrike orientation and nonzero angles measured clockwise
;                       (turns the turret to the right). Used after receiving 
;                       TOKEN_EOS in the TURR_ABS_ROT_DIGIT state. 		
;
; Operation:            This function first checks if the shared variable sign 
;                       indicates the command will be negative. If so, this is a 
;                       parsing error since only non-negative values are 
;                       accepted and the function SetParseError will be called to 
;                       set the ErrorFlag. If sign indicates the command is positive, 
;                       the command argument specified by Num will be set as AX
;                       (absolute angle argument in degrees) and passed into 
;                       function SetTurretAngle to rotate the turret to the 
;                       absolute angle. The parser is reset at the end so that it
;                       can start parsing new input.                		
;
; Arguments:		None 
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	Num - argument to be used in SetTurretAngle function (read) 
;                       Sign - indicates sign of the incoming command (read)   
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	If a negative value is about to be used for absolute 
;                       turret angle, a parsing error is indicated.  
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    AX, flags 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetTurrAngleAction      PROC        NEAR
                        PUBLIC      SetTurrAngleAction 
                        
CheckTurrAngleSign:
	CMP     Sign, NEGATIVE	;check if incoming command is negative 
	JE      TurrAngleError 	;if so, set ErrorFlag since we only accept
                                ;non-negative values since this is the 
                                ;absolute turret angle 
	;JNE	TurrAngleSuccess    ;if not, then we have a valid command and can
                                ;successfully set the turret angle 

AbsAngleSuccess:
	MOV     AX, Num	        ;set up AX as the absolute turret angle which
                                ;is required argument for function 
                                ;SetTurretAngle 
	CALL	SetTurretAngle	;call function SetTurretAngle with absolute 
                                ;turret angle to set the turret angle 
	CALL	ResetParseHandler ;reset parser so we can start parsing new command 
	JMP     TurrAngleDone   ;done setting the absolute turret angle 

TurrAngleError:
	CALL	SetParseError   ;call function SetParseError to indicate that
                                ;there was an error while parsing 
	;JMP	TurrAngleDone      

TurrAngleDone:
	RET		        ;done, so return 		

SetTurrAngleAction	ENDP



; SetRelTurrAngleAction
;
; Description:	        This function rotates the turret by the relative angle
;                       specified by the shared variable Num and the shared 
;                       variable Sign. Num is interpreted as a signed relative 
;                       angle in degrees through which to turn the turret relative 
;                       to the current turret position after processing Sign. 
;                       A relative angle of zero indicates no movement, positive 
;                       relative angles indicate clockwise rotation (turns the 
;                       turret to the right), and negative relative angles 
;                       indicate counter-clockwise rotation (turns the turret 
;                       to the left) Used after receiving TOKEN_EOS in the 
;                       TURR_REL_ROT_DIGIT state. 	  		
;
; Operation:		This function first checks the shared variable Sign to 
;                       determine if the relative angle is positive or negative. 
;                       If the relative angle is negative, Num is negated 
;                       in order to get the negative relative angle. Then we will
;                       call function SetRelTurretAngle with AX set up as the 
;                       relative angle to rotate the turret by the relative angle. 
;                       The parser is reset at the end so that it can start parsing
;                       new input. 
;
; Arguments:	        None 	
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	Num - argument to be used in SetRelTurretAngle function (read) 
;                       Sign - indicates sign of the incoming command (read)  
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
; Registers Changed:    AX, flags 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetRelTurrAngleAction   PROC        NEAR
                        PUBLIC      SetRelTurrAngleAction 
                        
CheckRelTurrAngleSign:
	MOV     AX, Num		    ;set up AX as the relative turret angle which
                                    ;is required argument for function 
                                    ;SetRelTurretAngle 
        CMP     Sign, POSITIVE      ;check if the relative angle is positive
        JE      RelTurrAngleSuccess ;if the relative angle is positive, then 
                                    ;we can go ahead and set the relative angle 
        ;JNE     NegativeRelTurrAngle   ;if the relative angle is negative, then
                                        ;we need to get the negative relative 
                                        ;turret angle 
    
NegativeRelTurrAngle:
        NEG     AX              ;negate the relative turret angle since 
                                ;the relative turret angle should be negative 
        ;JMP    RelTurrAngleSucess  ;now we can set the relative turret angle 

RelTurrAngleSuccess: 
	CALL	SetRelTurretAngle ;call function SetRelTurretAngle with 
                                  ;relative turret angle to rotate the 
                                  ;turret by the relative angle 
	CALL	ResetParseHandler ;reset parser so we can start parsing new
                                  ;command 
	RET			  ;done, so return 

SetRelTurrAngleAction	ENDP



; SetTurrEleAction
;
; Description:	        This function sets the absolute turret elevation angle 
;                       specified by the shared variable Num and the shared
;                       variable Sign. Num is interpreted as a signed angle in degrees 
;                       relative to the horizontal at which the turret is to be
;                       pointed up or down. A positive angle indicates the 
;                       the turret should be elevated up, a negative angle 
;                       indicates the turret should be elevated down, and a zero
;                       angle indicates horizontal elevation. Num should always 
;                       be between MAX_ELEVATION_ANGLE and -MAX_ELEVATION_ANGLE.
;                       Used after receiving TOKEN_EOS in the TOKEN_ELE_DIGIT state. 	  		
;
; Operation:		The function first sets up the turret elevation angle in 
;                       AX (signed angle argument to be passed into SetTurretElevation).
;                       Then the function checks if this absolute elevation angle is 
;                       within MAX_ELEVATION_ANGLE. If it is greater, then this is 
;                       a parsing error since the function SetTurretElevation only
;                       accepts angles between MAX_ELEVATION_ANGLE and 
;                       -MAX_ELEVATION_ANGLE and the function SetParseError will be 
;                       called to set the ErrorFlag. If the angle is within the 
;                       proper bounds, then the function checks if the signed
;                       elevation angle is negative or positive. If the elevation
;                       angle should be negative, Num is negated in order to 
;                       get the negative elevation angle. Then we will call the
;                       function SetTurretElevation with AX set up as the 
;                       elevation angle to elevated by. The parser is reset at the 
;                       end so that it can start parsing new input. 
;
; Arguments:		None 
;
; Return Value:		None  
;
; Local Variables:	None
;
; Shared Variables:	Num - argument to be used in SetTurretElevation function (read) 
;                       Sign - indicates sign of the incoming command (read)   
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	If the angle is not within MAX_ELEVATION_ANGLE and 
;                       -MAX_ELEVATION_ANGLE bounds, a parsing error is indicated.  
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    AX, flags 	
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/29/15
;

SetTurrEleAction      	PROC        NEAR
                        PUBLIC      SetTurrEleAction 

CheckTurrEleMax:
	MOV     AX, Num             ;set up AX is the turret elevation angle 
                                    ;which is required argument for function
                                    ;SetTurretElevaton 
	CMP 	AX, MAX_ELEVATION_ANGLE ;check to make sure this angle is within 
                                    ;the proper bounds 
	JG      TurrEleError        ;if the angle is above the bound, set 
                                    ;ErrorFlag since we only accept angles
                                    ;from MAX_ELEVATION_ANGLE to 
                                    ;-MAX_ELEVATION_ANGLE 
	;JLE	CheckTurrEleSign    ;now we must check if we elevate the 
                                    ;turret up or down 
                                    
CheckTurrEleSign:
	CMP     Sign, NEGATIVE	    ;check if the turret elevation angle is 
                                    ;negative 
	JNE     TurrEleSuccess      ;if not we can set the turret elevation angle
	;JE     NegateTurrEle       ;if negative, we need to get the negative
                                    ;turret elevation angle 

NegateTurrEle:
	NEG     AX		    ;negate the turret elevation angle since
                                    ;the turret angle should be negative 
	;JMP	TurrEleSuccess      ;now we can set the turret elevation angle 

TurrEleSuccess:
	CALL	SetTurretElevation  ;call function SetTurretElevation with 
                                    ;signed turret elevation angle to elevate
                                    ;turret up or down 
	CALL	ResetParseHandler   ;reset parse so we can start parsing new
                                    ;command 
	JMP     TurrEleDone         ;done setting turret elevation angle 

TurrEleError:
	CALL	SetParseError	    ;call function SetParseError to indicate 
                                    ;that there was an error while parsing 
	;JMP	TurrEleDone 	

TurrEleDone:
	RET			    ;done, so return 

SetTurrEleAction	ENDP



; StateTable
; 
; Description:	This is the state transition table for the Mealy state
;		machine. Each entry consists of the next state and actions
;		for that transition. The rows are associated with the 
;		current state and the columns with the input type. The 
;               table is organized in order of the states of the Mealy 
;               state machine and the entries for each state are organized
;               in order of the token types. 
; 
; Author: Yuan Ma
; Last Modified: 11/28/15


TRANSITION_ENTRY	STRUC	        ;structure used to define a table 	
	NEXTSTATE       DB	? 	;the next state for the transition
	ACTION	        DW	? 	;action for the transition
TRANSITION_ENTRY	ENDS 


;define a macro to make table a little more readable
;macro just does an offset of the action routine entries to build the STRUC
%*DEFINE(TRANSITION(next_state, act1))   (
	TRANSITION_ENTRY< %next_state, OFFSET(%act1) > 
) 


StateTable 	LABEL	TRANSITION_ENTRY

	;Current State = ST_INITIAL		Input Token Type
	%TRANSITION(ABS_SPEED, DoNothing)	; TOKEN_ABS_SPD
	%TRANSITION(REL_SPEED, DoNothing)	; TOKEN_REL_SPD 
	%TRANSITION(DIRECTION, DoNothing) 	; TOKEN_DIRECTION
	%TRANSITION(TURR_ELE, DoNothing) 	; TOKEN_TURR_ELE
	%TRANSITION(TURR_ROT, DoNothing)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_LASER_ON, DoNothing)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_LASER_OFF, DoNothing)	; TOKEN_LASER_OFF 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(ST_INITIAL, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, DoNothing)  	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = ST_ERROR		Input Token Type
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, DoNothing) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, DoNothing) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_LASER_OFF 
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_SIGN
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, ResetParseHandler) ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, DoNothing)	; TOKEN_OTHER

	;Current State = ABS_SPEED		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(ABS_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ABS_SPEED_SIGN, SetSignAction)   ; TOKEN_SIGN
	%TRANSITION(ABS_SPEED, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = ABS_SPEED_DIGIT	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(ABS_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(ABS_SPEED_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetAbsSpeedAction) ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = ABS_SPEED_SIGN		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(ABS_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(ABS_SPEED_SIGN, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = REL_SPEED		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(REL_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(REL_SPEED_SIGN, SetSignAction)   ; TOKEN_SIGN
	%TRANSITION(REL_SPEED, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = REL_SPEED_DIGIT	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(REL_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(REL_SPEED_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetRelSpeedAction)   ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = REL_SPEED_SIGN		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(REL_SPEED_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(REL_SPEED_SIGN, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = DIRECTION		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(DIRECTION_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(DIRECTION_SIGN, SetSignAction)   ; TOKEN_SIGN
	%TRANSITION(DIRECTION, DoNothing)	    ; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = DIRECTION_DIGIT	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(DIRECTION_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(DIRECTION_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetDirectionAction)  ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = DIRECTION_SIGN		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(DIRECTION_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(DIRECTION_SIGN, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER
	
	;Current State = TURR_ELE		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_ELE_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(TURR_ELE_SIGN, SetSignAction)   ; TOKEN_SIGN
	%TRANSITION(TURR_ELE, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = TURR_ELE_DIGIT	        Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_ELE_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(TURR_ELE_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetTurrEleAction)  ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = TURR_ELE_SIGN	        Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_ELE_DIGIT, AddDigitAction) ; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(TURR_ELE_SIGN, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = TURR_ROT		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_ABS_ROT_DIGIT, AddDigitAction)	; TOKEN_DIGIT
	%TRANSITION(TURR_REL_ROT_SIGN, SetSignAction)	; TOKEN_SIGN
	%TRANSITION(TURR_ROT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = TURR_ABS_ROT_DIGIT	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_ABS_ROT_DIGIT, AddDigitAction)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(TURR_ABS_ROT_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetTurrAngleAction)     ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = TURR_REL_ROT_SIGN  	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_REL_ROT_DIGIT, AddDigitAction)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(TURR_REL_ROT_SIGN, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetParseError) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER
    
    ;Current State = TURR_REL_ROT_DIGIT  	Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(TURR_REL_ROT_DIGIT, AddDigitAction)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(TURR_REL_ROT_DIGIT, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, SetRelTurrAngleAction) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = ST_LASER_ON		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(ST_LASER_ON, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, LaserOnAction) 	; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER

	;Current State = ST_LASER_OFF		Input Token Type
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_ABS_SPD
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_REL_SPD 
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_DIRECTION
	%TRANSITION(ST_ERROR, SetParseError) 	; TOKEN_TURR_ELE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_TURR_ROT 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_FIRE
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_LASER_OFF 
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_DIGIT
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_SIGN
	%TRANSITION(ST_LASER_OFF, DoNothing)	; TOKEN_IGNORE 
	%TRANSITION(ST_INITIAL, LaserOffAction) ; TOKEN_EOS	
	%TRANSITION(ST_ERROR, SetParseError)	; TOKEN_OTHER


; GetToken
;
; Description:      This procedure returns the token class and token value for
;                   the passed character.  The character is truncated to
;                   7-bits.
;
; Operation:        Looks up the passed character in two tables, one for token
;                   types or classes, the other for token values.
;
; Arguments:        AL - character to look up.
; Return Value:     AL - token value for the character.
;                   AH - token type or class for the character.
;
; Local Variables:  BX - table pointer, points at lookup tables.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       Table lookup.
; Data Structures:  Two tables, one containing token values and the other
;                   containing token types.
;
; Registers Used:   AX, BX.
; Stack Depth:      0 words.
;
; Author:           Glen George, Yuan Ma 
; Last Modified:    11/28/15

GetToken	PROC    NEAR


InitGetToken:				    ;setup for lookups
	AND     AL, TOKEN_MASK		    ;strip unused bits (high bit)
	MOV     AH, AL			    ;and preserve value in AH


TokenTypeLookup:                            ;get the token type
        MOV     BX, OFFSET(TokenTypeTable)  ;BX points at table
	XLAT	CS:TokenTypeTable           ;have token type in AL
	XCHG	AH, AL		            ;token type in AH, character in AL

TokenValueLookup:			    ;get the token value
        MOV     BX, OFFSET(TokenValueTable) ;BX points at table
	XLAT	CS:TokenValueTable	    ;have token value in AL

EndGetToken:                     	    ;done looking up type and value
    RET


GetToken	ENDP



; Token Tables
;
; Description:      This creates the tables of token types and token values.
;                   Each entry corresponds to the token type and the token
;                   value for a character.  Macros are used to actually build
;                   two separate tables - TokenTypeTable for token types and
;                   TokenValueTable for token values.
;
; Author:           Glen George, Yuan Ma
; Last Modified:    11/28/15

%*DEFINE(TABLE)  (
        %TABENT(TOKEN_OTHER, 0)		;<null>  
        %TABENT(TOKEN_OTHER, 1)		;SOH
        %TABENT(TOKEN_OTHER, 2)		;STX
        %TABENT(TOKEN_OTHER, 3)		;ETX
        %TABENT(TOKEN_OTHER, 4)		;EOT
        %TABENT(TOKEN_OTHER, 5)		;ENQ
        %TABENT(TOKEN_OTHER, 6)		;ACK
        %TABENT(TOKEN_OTHER, 7)		;BEL
        %TABENT(TOKEN_OTHER, 8)		;backspace
        %TABENT(TOKEN_IGNORE, 9)	;TAB (ignored) 
        %TABENT(TOKEN_OTHER, 10)	;new line
        %TABENT(TOKEN_OTHER, 11)	;vertical tab
        %TABENT(TOKEN_OTHER, 12)	;form feed
        %TABENT(TOKEN_EOS, 13)		;carriage return (end of command indicator) 
        %TABENT(TOKEN_OTHER, 14)	;SO
        %TABENT(TOKEN_OTHER, 15)	;SI
        %TABENT(TOKEN_OTHER, 16)	;DLE
        %TABENT(TOKEN_OTHER, 17)	;DC1
        %TABENT(TOKEN_OTHER, 18)	;DC2
        %TABENT(TOKEN_OTHER, 19)	;DC3
        %TABENT(TOKEN_OTHER, 20)	;DC4
        %TABENT(TOKEN_OTHER, 21)	;NAK
        %TABENT(TOKEN_OTHER, 22)	;SYN
        %TABENT(TOKEN_OTHER, 23)	;ETB
        %TABENT(TOKEN_OTHER, 24)	;CAN
        %TABENT(TOKEN_OTHER, 25)	;EM
        %TABENT(TOKEN_OTHER, 26)	;SUB
        %TABENT(TOKEN_OTHER, 27)	;escape
        %TABENT(TOKEN_OTHER, 28)	;FS
        %TABENT(TOKEN_OTHER, 29)	;GS
        %TABENT(TOKEN_OTHER, 30)	;AS
        %TABENT(TOKEN_OTHER, 31)	;US
        %TABENT(TOKEN_IGNORE, ' ')	;space (ignored) 
        %TABENT(TOKEN_OTHER, '!')	;!
        %TABENT(TOKEN_OTHER, '"')	;"
        %TABENT(TOKEN_OTHER, '#')	;#
        %TABENT(TOKEN_OTHER, '$')	;$
        %TABENT(TOKEN_OTHER, 37)	;percent
        %TABENT(TOKEN_OTHER, '&')	;&
        %TABENT(TOKEN_OTHER, 39)	;'
        %TABENT(TOKEN_OTHER, 40)	;open paren
        %TABENT(TOKEN_OTHER, 41)	;close paren
        %TABENT(TOKEN_OTHER, '*')	;*
        %TABENT(TOKEN_SIGN, +1)		;+  (positive sign)
        %TABENT(TOKEN_OTHER, 44)	;,
        %TABENT(TOKEN_SIGN, -1)		;-  (negative sign)
        %TABENT(TOKEN_OTHER, 0)		;.  
        %TABENT(TOKEN_OTHER, '/')	;/
        %TABENT(TOKEN_DIGIT, 0)		;0  (digit)
        %TABENT(TOKEN_DIGIT, 1)		;1  (digit)
        %TABENT(TOKEN_DIGIT, 2)		;2  (digit)
        %TABENT(TOKEN_DIGIT, 3)		;3  (digit)
        %TABENT(TOKEN_DIGIT, 4)		;4  (digit)
        %TABENT(TOKEN_DIGIT, 5)		;5  (digit)
        %TABENT(TOKEN_DIGIT, 6)		;6  (digit)
        %TABENT(TOKEN_DIGIT, 7)		;7  (digit)
        %TABENT(TOKEN_DIGIT, 8)		;8  (digit)
        %TABENT(TOKEN_DIGIT, 9)		;9  (digit)
        %TABENT(TOKEN_OTHER, ':')	;:
        %TABENT(TOKEN_OTHER, ';')	;;
        %TABENT(TOKEN_OTHER, '<')	;<
        %TABENT(TOKEN_OTHER, '=')	;=
        %TABENT(TOKEN_OTHER, '>')	;>
        %TABENT(TOKEN_OTHER, '?')	;?
        %TABENT(TOKEN_OTHER, '@')	;@
        %TABENT(TOKEN_OTHER, 'A')	;A
        %TABENT(TOKEN_OTHER, 'B')	;B
        %TABENT(TOKEN_OTHER, 'C')	;C
        %TABENT(TOKEN_DIRECTION, 'D')	;D  (set direction indicator) 
        %TABENT(TOKEN_TURR_ELE, 'E')	;E  (set turret elevation angle indicator) 
        %TABENT(TOKEN_LASER_FIRE, 'F')	;F  (fire laser indicator) 
        %TABENT(TOKEN_OTHER, 'G')	;G
        %TABENT(TOKEN_OTHER, 'H')	;H
        %TABENT(TOKEN_OTHER, 'I')	;I
        %TABENT(TOKEN_OTHER, 'J')	;J
        %TABENT(TOKEN_OTHER, 'K')	;K
        %TABENT(TOKEN_OTHER, 'L')	;L
        %TABENT(TOKEN_OTHER, 'M')	;M
        %TABENT(TOKEN_OTHER, 'N')	;N
        %TABENT(TOKEN_LASER_OFF, 'O')   ;O  (turn laser off indicator) 
        %TABENT(TOKEN_OTHER, 'P')	;P
        %TABENT(TOKEN_OTHER, 'Q')	;Q
        %TABENT(TOKEN_OTHER, 'R')	;R
        %TABENT(TOKEN_ABS_SPD, 'S')	;S  (set absolute speed indicator) 
        %TABENT(TOKEN_TURR_ROT, 'T')    ;T  (rotate turret angle indicator ) 
        %TABENT(TOKEN_OTHER, 'U')	;U
        %TABENT(TOKEN_REL_SPD, 'V')	;V  (set relative speed indicator)
        %TABENT(TOKEN_OTHER, 'W')	;W
        %TABENT(TOKEN_OTHER, 'X')	;X
        %TABENT(TOKEN_OTHER, 'Y')	;Y
        %TABENT(TOKEN_OTHER, 'Z')	;Z
        %TABENT(TOKEN_OTHER, '[')	;[
        %TABENT(TOKEN_OTHER, '\')	;\
        %TABENT(TOKEN_OTHER, ']')	;]
        %TABENT(TOKEN_OTHER, '^')	;^
        %TABENT(TOKEN_OTHER, '_')	;_
        %TABENT(TOKEN_OTHER, '`')	;`
        %TABENT(TOKEN_OTHER, 'a')	;a
        %TABENT(TOKEN_OTHER, 'b')	;b
        %TABENT(TOKEN_OTHER, 'c')	;c
        %TABENT(TOKEN_DIRECTION, 'd')	;d  (set direction indicator) 
        %TABENT(TOKEN_TURR_ELE, 'e')	;e  (set turret elevation angle indicator) 
        %TABENT(TOKEN_LASER_FIRE, 'f')	;f  (fire laser indicator) 
        %TABENT(TOKEN_OTHER, 'g')	;g
        %TABENT(TOKEN_OTHER, 'h')	;h
        %TABENT(TOKEN_OTHER, 'i')	;i
        %TABENT(TOKEN_OTHER, 'j')	;j
        %TABENT(TOKEN_OTHER, 'k')	;k
        %TABENT(TOKEN_OTHER, 'l')	;l
        %TABENT(TOKEN_OTHER, 'm')	;m
        %TABENT(TOKEN_OTHER, 'n')	;n
        %TABENT(TOKEN_LASER_OFF, 'o')   ;o  (turn laser off indicator) 
        %TABENT(TOKEN_OTHER, 'p')	;p
        %TABENT(TOKEN_OTHER, 'q')	;q
        %TABENT(TOKEN_OTHER, 'r')	;r
        %TABENT(TOKEN_ABS_SPD, 's')	;s  (set absolute speed indicator) 
        %TABENT(TOKEN_TURR_ROT, 't')    ;t  (rotate turret angle indicator) 
        %TABENT(TOKEN_OTHER, 'u')	;u
        %TABENT(TOKEN_REL_SPD, 'v')	;v  (set relative speed indicator) 
        %TABENT(TOKEN_OTHER, 'w')	;w
        %TABENT(TOKEN_OTHER, 'x')	;x
        %TABENT(TOKEN_OTHER, 'y')	;y
        %TABENT(TOKEN_OTHER, 'z')	;z
        %TABENT(TOKEN_OTHER, '{')	;{
        %TABENT(TOKEN_OTHER, '|')	;|
        %TABENT(TOKEN_OTHER, '}')	;}
        %TABENT(TOKEN_OTHER, '~')	;~
        %TABENT(TOKEN_OTHER, 127)	;rubout
)

; token type table - uses first byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokentype
)

TokenTypeTable	LABEL   BYTE
        %TABLE


; token value table - uses second byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokenvalue
)

TokenValueTable	LABEL   BYTE
        %TABLE	


CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'

CurrentState	        DB	?	;current state of mealy state machine 
                                        ;that the parser is at 
Sign		        DB	?	;indicates sign of the incoming command 
Num		        DW	?	;argument to be used in the action functions 
ErrorFlag	        DB	? 	;indicates if there was an error while parsing 
ParseSuccessFlag        DB      ?       ;indicates what was parsed 

DATA    ENDS



        END       
