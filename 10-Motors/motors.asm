        NAME    MOTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                     MOTORS                                 ;
;                                Motor Functions                             ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; File Description: 
; 	This file contains the motor functions for the RoboTrike and their  
;	functional specifications: InitMotorLaser, SetMotorSpeed, 
;	GetMotorSpeed, GetMotorDirection, SetLaser, GetLaser and 
;	PWMEventHandler. These functions set the speed and direction of the
;	RoboTrike motors using pulse width modulation and control the RoboTrike 
;	laser. They also allow the current status of the RoboTrike’s speed, 
;	direction, and laser to be read. 
;	
;
; Table of Contents:
;	InitMotorLaser: 	    initializes the shared variables for the 
;				    motors, lasers, and parallel I/O values 
;	SetMotorSpeed:		    set the speed and direction of the RoboTrike 
;	GetMotorSpeed:		    get the current speed setting for the RoboTrike 
;	GetMotorDirection:	    get the current direction of movement
;				    setting for the RoboTrike 
;	SetLaser:		    turn the laser on or off 
;	GetLaser:		    get the current laser status 
;	PWMEventHandler: 	    uses pulse width modulation on the motors
;				    when called by the timer1 event handler  
;
;   Tables:
;	ForceX_Table:		    Table of values for the x-component of each
;				    motor’s force
;	ForceY_Table:		    Table of values for the y-component of each 
;				    motor’s force 
;	Motor_Bit_Table:            Table of bits that should be set for each 
;				    motor to be on and moving forward or backward 
;
;   Stub Functions:
;   SetTurrentAngle
;   SetRelTurretAngle
;   SetTurretElevation 
;
;
; Revision History:
;     11/9/15  	Yuan Ma     wrote outline
;     11/13/15	Yuan Ma	    wrote code and comments and updated functional
;			    specification for functions
;     11/14/15  Yuan Ma     started debugging, updated comments 
;     11/15/15  Yuan Ma     finished debugging, updated comments 
;     12/11/15  Yuan Ma     added stub functions 


;local include files
$INCLUDE(motors.inc) 
$INCLUDE(timer1.inc) 

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

	;tables of sine and cosine values normalized for 
	;calculation of the motor angles and speeds 
	EXTRN	Sin_Table:WORD		;sine value table 
	EXTRN	Cos_Table:WORD		;cosine value table 

		
; ForceX_Table  
;
; Description: 	This is the table for the direction of each 
;		motor’s force in the x-component in Q(0.15) 
;		fixed-point format. 
; 
; Notes:	READ ONLY tables should always be in the code 
;		segment so that in a standalone system it will be 
;		located in the ROM with the code 
;
; Author: Yuan Ma
; Last Modified: 11/13/15

ForceX_Table	LABEL	WORD

;	DW	Motor force’s x-component 

	DW	MOTOR0_X	;Motor 1 x-component 
	DW	MOTOR1_X	;Motor 2 x-component 
	DW	MOTOR2_X	;Motor 3 x-component 


; ForceY_Table  
;
; Description: 	This is the table for the direction of each 
;		motor’s force in the y-component in Q(0.15) 
;		fixed-point format. 
; 
; Notes:	READ ONLY tables should always be in the code 
;		segment so that in a standalone system it will be 
;		located in the ROM with the code 
;
; Author: Yuan Ma
; Last Modified: 11/13/15

ForceY_Table	LABEL	WORD

;	DW	Motor force’s y-component 

	DW	MOTOR0_Y 	;Motor 1 y-component
	DW	MOTOR1_Y	;Motor 2 y-component 
	DW	MOTOR2_Y        ;Motor 3 y-component 

; Motor_Bit_Table
;
; Description:	This is the table that gives the bits for each 
;		motor to be on and moving either forward or backward 
;
; Notes:	READ ONLY tables should always be in the code 
;		segment so that in a standalone system it will
;		be located in the ROM with the code 
;
; Author: Yuan Ma
; Last Modified: 11/14/15

Motor_Bit_Table 	LABEL	BYTE

;	DB	bits that will be set for each motor

	DB	MOTOR0_FORWARD 	    ;Motor 0 on and moving forward 
	DB	MOTOR0_BACKWARD     ;Motor 0 on and moving backward
	DB	MOTOR1_FORWARD      ;Motor 1 on and moving forward
	DB	MOTOR1_BACKWARD     ;Motor 1 on and moving backward 
	DB	MOTOR2_FORWARD      ;Motor 2 on and moving forward
	DB	MOTOR2_BACKWARD     ;Motor 2 on and moving backward 



; InitMotorLaser  
;
; Description:		This function initializes the shared variables for 
;			the RobotTrike motors speed, direction, and laser 
;			and initializes the variables for the pulse width 
;			modulation. The RoboTrike is initially set to be off
;		        while facing straight ahead relative to the RoboTrike 
;		        orientation and the laser is set to be off. 
;
; Operation:		The function resets the motors and laser to be in the 
;                       off state. The function sets initial motor speed to be 
;			REST_SPEED, the initial motor direction to be 
;			MIN_ANGLE, the laser status to be LASER_OFF, and 
;			initializes the pulse width modulation for each 
;			motor by setting the pulse width counter to 
;			PW_COUNTER_END and adding a STOP_PULSE for each 
;			motor to the pulse width list. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	PulseWidthCounter - counter for the pulse width 
;					     (written to)
;			PulseWidths[] - list of pulse widths (written to)
;			MotorSpeed - speed of the RoboTrike (written to)
;		        MotorAngle - angle of the RoboTrike (written to)
;			LaserStatus - the status of the laser (written to)
;
; Global Variables:	None 
;
; Input:	        None
;
; Output:		Byte on Port B to turn motor/laser off 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    AX, DX, flags 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/13/15
;
 
InitMotorLaser       	PROC        NEAR
                        PUBLIC      InitMotorLaser 
	
InitParallelPortB:
	MOV     DX, MOTOR_PORT_ADDRESS 	;Port B is set up as as the output
	MOV     AL, LASER_MOTORS_OFF    ;initially turn all the motors off 
                                        ;and turn the laser off 		
	OUT     DX, AL 			             
	;JMP	InitMotorVariables

InitMotorVariables: 
	MOV     MotorSpeed, REST_SPEED	;initialize speed to no movement 
	MOV     MotorAngle, MIN_ANGLE	;initialize angle to straight ahead
                                        ;relative to RoboTrike orientation 
	MOV	LaserStatus, LASER_OFF	;initialize laser to be off 
	MOV	PulseWidthCounter, PW_COUNTER_END	;initialize pulse width counter 
	
	MOV     PulseWidths[FIRST_MOTOR], STOP_PULSE    ;add a zero pulse for 
        MOV     PulseWidths[SECOND_MOTOR], STOP_PULSE   ;each motor to the pulse
        MOV     PulseWidths[THIRD_MOTOR], STOP_PULSE    ;width list 
    
	RET				

InitMotorLaser 		ENDP


; SetMotorSpeed 
;
; Description:		This function sets the speed and direction of the 
;			RoboTrike. It is passed in the absolute speed (AX) and 
;			signed angle  (BX) that we wish to set the RoboTrike  
;		        to. If the speed is IGNORE_SPEED, this 
;			indicates the current speed should not be changed. 
;			If the angle is IGNORE_ANGLE, this 
;			indicates the current direction of travel should not 
;			be changed. The angle is measured clockwise in degrees 
;                       with an angle of MIN_ANGLE representing straight ahead 
;			relative to the RoboTrike orientation.  
;
; Operation:		The function first checks if the speed is not 
;			IGNORE_SPEED. If it isn’t, is uses the passed in 
;			speed from AX; if it is, it ignores the speed and moves
;                       on to checking the angle. Then the function checks 
;			if the angle is the IGNORE_ANGLE. If it is, it 
;                       ignores the passed in angle and moves on to calculating
;                       a pulse width. If it isn't, the function will 
;                       adjust the passed in angle by setting the (angle = 
;                       angle mod (MAX_ANGLE)) and adding MAX_ANGLE if the 
;                       resulting angle is negative. Once the function has
;                       the correct angle and speed, it will move on to 
;                       calculating a pulse width for each motor through 
;                       fixed point arithmetic. The pulse width is calculated 
;                       by the equation: speed = (ForceX * v * cos(angle)) + 
;			(ForceY * v * sin(angle)). The force vectors and sine   
;                       and cosine values are obtained through table lookup.  
;
; Arguments:		speed (AX) - tells what speed the RoboTrike should
;				     be set to. If it is IGNORE_SPEED, 
;				     the speed won’t be changed
;			angle (BX) - tells what direction the RoboTrike 
;				     should be set to. It is a signed angle.
;				     If it is IGNORE_ANGLE, the direction 
;                                       won’t be changed 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	MotorSpeed - speed of the RoboTrike (written to)
;			MotorAngle - angle of the RoboTrike (written to) 
;			PulseWidths[] - list of motor pulse widths (written to/read) 
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
; Limitations:		Fixed-point arithmetic has 7 bit limitation for 
;                       accuracy (and one sign bit) 	
;
; Author: Yuan Ma 
; Last Modified: 11/15/15
;

SetMotorSpeed       	PROC        NEAR
                        PUBLIC      SetMotorSpeed

	
    PUSHA               ;save all the registers 
    
CheckSpeed:
	CMP	    AX, IGNORE_SPEED	;check if we should ignore the speed 
	JE		CheckAngle	        ;if it is the ignore speed, we ignore 
                                ;and move on to checking the angle
	;JNE	SetNewSpeed	        ;it it is not the ignore speed, we will 
                                ;set the new speed as the passed in speed  

SetNewSpeed:
	MOV	    MotorSpeed, AX		;set the speed to be the passed in speed
	;JMP	CheckAngle		    ;now we need to check the angle 

CheckAngle:
	CMP	    BX, IGNORE_ANGLE	;check if we should ignore the angle
	JE	    CalcSpeedsInit	    ;if it the ignore angle, we ignore and move on 
                                ;to start calculating the speed 
	;JNE	SetNewAngle		    ;if it is not the ignore angle, we will 
                                ;we need to set the new angle  

SetNewAngle:
	MOV	    AX, BX              ;move the angle into AX so we can convert it
                                ;into a signed double word 
	CWD				            ;convert the angle into a signed double
                                ;word, now half of it is in DX
	MOV	    BX, MAX_ANGLE		;we need to make sure the angle is between
                                ;the MIN_ANGLE and MAX_ANGLE
	IDIV	BX			        ;Divide by the maximum angle, the remainder 
                                ;(what we want: angle = angle mod MAX_ANGLE)
                                ;is now in DX for double word division 
	CMP	    DX, 0 		        ;check if angle is negative or not 				
	JGE	    SetNewAngleDone		;if the angle is positive, we are done
                                ;and can set it to be MotorAngle
	;JL	    CorrectNegAngle		;if the angle is negative, we need to add
                                ;MAX_ANGLE to make sure it is within the range
					
CorrectNegAngle:
	ADD	    DX, MAX_ANGLE		;adding MAX_ANGLE will give us the same 
                                ;angle, but positive 
	;JMP	SetNewAngleDone

SetNewAngleDone:
	MOV	    MotorAngle, DX 		;set the new angle to be MotorAngle 
	;JMP	CalcSpeedsInit		;now that we have the correct angle and
                                ;speed we can calculate a speed 
CalcSpeedsInit:
	MOV	    BX, MotorAngle      ;move the MotorAngle into BX for calculations 
	ADD	    BX, BX				;shift the angle because the sine/cosine 
                                ;values are words 
	MOV	    DI, FIRST_MOTOR		;begin looping from the first motor  
	;JMP	CalcSpeedsLoop

CalcSpeedsLoop:
	CMP 	DI, NUM_MOTORS	    ;check if we have looped through all 
                                ;of the motors
	JAE	    SetSpeedDone		;if we have, then we are done adding pulses 
	;JL	    CalcSpeed		    ;if not, we need to calculate another
                                ;motor speed
CalcSpeed:  
	PUSH	BX			        ;save angle for next loop calculation
    PUSH    DI                  ;save the motor index 
    MOV	    AX, MotorSpeed      ;move the MotorAngle into AX for calculations 
	SHR	    AX, SHIFT_BIT 		;divide the speed by to remove the sign bit 
    PUSH    AX                  ;save the speed for when we calculate the 
                                ;y-direction 
	SAL	    DI, SHIFT_BIT 	    ;shift the motor index because the 
                                ;force tables are indexed as words 

	;calculate the everything in the x-direction
	IMUL	WORD PTR CS:ForceX_Table[DI] 	;We multiply the ForceX vector by 
                                            ;the speed (F_x * v) 
	MOV	    AX, DX 			                ;truncate to DX and store in AX 
	IMUL	WORD PTR CS:Cos_Table[BX] 	    ;Multiply by cosine of the angle 
                                            ;(F_x * v * cos(angle))
	MOV	    CX, DX			                ;truncate to DX and store in CX
    POP     AX                              ;restore the speed for the 
                                            ;y-direction calculations 
 
	;calculate everything in the y-direction 
	IMUL	WORD PTR CS:ForceY_Table[DI]	    ;We multiply the ForceY vector 
                                                ;by the speed (F_y * v)
	MOV	    AX, DX			                    ;truncate to DX 
	IMUL	WORD PTR CS:Sin_Table[BX] 	        ;Multiply by sine of the angle
                                                ;(F_y * v * sin(angle))
	;add x and y parts 
	ADD	    CX, DX 			        ;we add the x and y parts together to
                                    ;complete the calculation:
                                ;(F_x * v * cos(angle)) + (F_y * v * sin(angle))
	SAL	    CX, EXTRA_SIGN_BITS     ;get rid of extra sign bits since 
                                    ;multiplying three Q(0.15) numbers gives us
                                    ;extra sign bits, but we only need one 
	POP 	DI			            ;restore motor counter 
	POP 	BX			            ;restore angle 
	MOV	    BYTE PTR PulseWidths[DI], CH	    ;truncate the value we 
                                                ;calculated to CH and add it to 
                                                ;the PulseWidths list                                                     
    INC     DI                       			;increment to the next motor 
	JMP	    CalcSpeedsLoop		                ;loop back up 		

SetSpeedDone:
    POPA                            ;restore all registers 
	RET				

SetMotorSpeed		ENDP




; GetMotorSpeed
;
; Description:		This function returns the current speed setting for
;			        the RoboTrike in AX which must be between MIN_SPEED and 
;		        	MAX_SPEED, inclusively. The MIN_SPEED represents 
;			        when the RoboTrike is stopped.   		      
;
; Operation:		The function returns the shared variable MotorSpeed to AX.   
;
; Arguments:		None 
;
; Return Value:		AX - the current speed setting of the RoboTrike 
;
; Local Variables:	None
;
; Shared Variables:	MotorSpeed - speed of the RoboTrike (read from) 
;
; Global Variables:	None
;
; Input:		    None 
;
; Output:		    None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: AX
;
; Limitations:		None
;
; Author: Yuan Ma
; Last Modified: 11/13/15
;
;

GetMotorSpeed   PROC        NEAR
                PUBLIC      GetMotorSpeed

	MOV     AX, MotorSpeed	;return value of the stored RoboTrike speed in AX
 	RET


GetMotorSpeed	ENDP



; GetMotorDirection 
;
; Description:		This function returns the current direction of 
;			        movement setting for the RoboTrike in AX which 
;			        must be between MIN_ANGLE and MAX_ANGLE inclusively. 
;			        The direction is expressed as angle in degrees. An 
;			        angle of MIN_ANGLE represents straight ahead
;			        relative to the RoboTrike orientation and angles
;			        are measured clockwise.   		      
;
; Operation:		The function returns the shared variable MotorAngle to AX 
;
; Arguments:		None 
;
; Return Value:		AX - current direction of movement setting for the 
;			             RoboTrike 
;
; Local Variables:	None
;
; Shared Variables:	MotorAngle - angle of the RoboTrike (read from) 
;
; Global Variables:	None
;
; Input:		    None 
;
; Output:		    None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: AX
;
; Limitations:		None 
;
; Author: Yuan Ma
; Last Modified: 11/13/15
;


GetMotorDirection   	PROC        NEAR
                        PUBLIC      GetMotorDirection

	MOV	    AX, MotorAngle 	;return value of the stored RoboTrike direction in AX 
 	RET
 


GetMotorDirection	ENDP


; SetLaser
;
; Description:		This function turns the laser on or off depending
;			        on the passed in value (onoff) in AX. A zero value
;			        turns the laser off and a nonzero value turns the laser on.    		      
;
; Operation:		The function sets LaserStatus to be the passed in 
;			        value AX. Later, the PWMEventHandler will actually
;			        check if the value is nonzero or not and set the laser. 
;
; Arguments:		onoff(AX) - if zero, the laser is turned off; if 
;				                nonzero, the laser is turned on  
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	LaserStatus - the status of the laser (written to) 
;
; Global Variables:	None
;
; Input:		    None 
;
; Output:		    None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: AX
;
; Limitations:		None 	
;
; Author: Yuan Ma
; Last Modified: 11/13/15
; 

SetLaser   	PROC        NEAR
           	PUBLIC      SetLaser

	MOV	    LaserStatus, AX		;writes the passed in value to LaserStatus.
                                ;This value will be checked and the laser 
                                ;will be set in the PWMEventHandler 
	RET


SetLaser	ENDP



; GetLaser
;
; Description:		This function returns the status of the RoboTrike 
;			        laser in AX. A zero value indicates the laser is 
;			        off and a non-zero value indicates the laser is on.      
;
; Operation:		The function returns the shared variable LaserStatus to AX
;
; Arguments:		None 
;
; Return Value:		AX - laser status 
;
; Local Variables:	None
;
; Shared Variables:	LaserStatus - the status of the laser (read from) 
;
; Global Variables:	None
;
; Input:		    None 
;
; Output:		    None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: AX
;
; Limitations:		None 
;
; Author: Yuan Ma
; Last Modified: 11/13/15
;

GetLaser  	PROC        NEAR
            PUBLIC      GetLaser


	MOV	    AX, LaserStatus 	;return value of stored laser status in AX 
 	RET 


GetLaser	ENDP

; SetTurretAngle 

SetTurretAngle  PROC    NEAR
                PUBLIC  SetTurretAngle 
                
    RET

SetTurretAngle  ENDP 

; SetRelTurretAngle 

SetRelTurretAngle   PROC    NEAR
                    PUBLIC  SetRelTurretAngle 
                    
    RET

SetRelTurretAngle   ENDP

; SetTurretElevation 

SetTurretElevation  PROC    NEAR
                    PUBLIC  SetTurretElevation 

    RET
    
SetTurretElevation  ENDP



; PWMEventHandler 
;
; Description:		This function uses pulse width modulation on the 
;			        motors when called by the timer event handler and 
;			        turns the laser on or off. Previously, we calculated
;                   a pulse width in SetMotorSpeed which determines how
;                   long we want to turn the motor on for. In this event
;                   handler, we will check if the counter has reached this 
;                   pulse width so we can turn the motor on for this specified
;                   amount of time out of (MAX_PULSE). The duty cycle can 
;                   be found by dividing the pulse width by (MAX_PULSE)
;                   An interrupt is generated once every (PWMCYCLETIME / 
;                   SEVEN_BIT_RES) milliseconds. It will use Timer1, a different  
;                   timer than the one used for multiplexing and keypad scanning. 
;
; Operation:		The function will check the PulseWidthCounter to see
;                   if we have reached the end of the count. If so, 
;                   we need to reset the counter and turn all the motors 
;                   off; if not we will begin to loop through the motors 
;                   to see if they need to be turned on. The function will 
;                   obtain the pulse width of each motor from PulseWidths
;                   list and check if it is negative or positive so the 
;                   direction of the motor can be set. Then, it will check
;                   if the PulseWidthCounter is equal to the pulse width.
;                   If it is equal, the motor will be turned on and if 
;                   if it not equal, the function will loop to check the 
;                   next motor. The LaserStatus is also checked and the 
;                   laser is set to be on/off. The final bits that are 
;                   set will be outputted to Port B.   
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	PulseWidthCounter - counter for the pulse width 
;					                    (read from/written)
;			        PulseWidths[] - list of pulse widths for each motor
;					                (read from) 
;			        LaserStatus - indicated whether the laser is turned
;                                 on or off (read from) 
;
;
; Global Variables:	None 
;
; Input:		    None
;
; Output:		    Bits are set to parallel port B which turn the motors
;                   on/off and the laser on/off  
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed: None 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/15/15
;

PWMEventHandler       PROC        NEAR
                      PUBLIC      PWMEventHandler

	

	PUSHA					;push all registers 

CheckPWCounter:
	CMP	    PulseWidthCounter, PW_COUNTER_END     ;check if PulseWidthCounter
                                                  ;has counted down to the end
	JNE	    UpdateCounterLoopInit                 ;if it hasn't, we check if 
                                                  ;the motors need to be turned
                                                  ;on 
	;JE	    ResetCounter                          ;if it is, reset the counter   
    
ResetCounter:
	MOV	    PulseWidthCounter, MAX_PULSE        ;reset PulseWidthCounter to 
                                                ;start counting down from the
                                                ;MAX_PULSE
	AND	    PortBValue, MOTORS_OFF              ;turn all the motors off
	JMP	    CheckLaser                          ;move on to checking the laser 

UpdateCounterLoopInit:
	MOV	    BX, FIRST_MOTOR                 ;start looping from first motor
    ;JMP    GetPulseWidth
    
GetPulseWidth:                              
	MOV	    AL, BYTE PTR PulseWidths[BX]	;obtain the pulse width for the 
                                            ;corresponding motor from the 
                                            ;PulseWidths list 
	;JMP	MotorDirectionCheck             ;check the pulse width direction 

MotorDirectionCheck:      
    CMP     AL, 0                           ;check if the pulse width is negative 
    JGE     SetForwardDirection             ;if position, set the direction to 
                                            ;be forward 
    ;JL     SetBackwardDirection            ;if negative, set the direction to 
                                            ;be backward 

SetBackwardDirection:
    MOV     DL, BACKWARD_DIRECTION          ;set direction to be backward
    NEG     AL                              ;take absolute value of pulse width 
    JMP     CheckMotor                      ;move on to checking the motors 
                                            ;note: Reverse 100% won't work 
    
SetForwardDirection:    
    MOV     DL, FORWARD_DIRECTION           ;set direction to be forward 
    ;JMP    CheckMotor                      ;move on to checking the motors 
    
CheckMotor:
    CMP     PulseWidthCounter, AL           ;check if pulse width is equal to 
                                            ;the PulseWidthCounter
    JNE     UpdateCounterLoop               ;if not equal, loop to check next motor
    ;JE     TurnMotorOn                     ;if it is equal, turn that motor on 

TurnMotorOn:
    PUSH    BX                              ;save motor index 
    ADD     BL, BL                          ;multiply the index by two 
    ADD     BL, DL                          ;add the direction 
    MOV     CL, BYTE PTR CS:Motor_Bit_Table[BX]    ;look up which motor to turn on 
    OR      PortBValue, CL                          ;OR the bits so we can get
                                                    ;our final value without 
                                                    ;changing the other bits 
    POP     BX                              ;restore the motor index 
    
UpdateCounterLoop:
    INC     BX                              ;increment to check the next motor
    CMP     BX, NUM_MOTORS                  ;have we reached the last motor?
    JNE     GetPulseWidth                   ;if not, check the next motor 
   ;JE      CheckLaser                      ;if so, move on to checking laser 
   
CheckLaser:
    CMP     LaserStatus, LASER_OFF          ;check the laser status to see if
                                            ;it is off 
    JE      SetLaserOff                     ;if it is, turn the laser off 
    ;JNE    SetLaserOn                      ;if it isn't, turn the laser on 

SetLaserOn:
    OR      PortBValue, SET_LASER_ON        ;turn laser on 
    JMP     OutputPortBValue 
    
SetLaserOff:
    AND     PortBValue, SET_LASER_OFF       ;turn laser off 
    ;JMP    OutputPortBValue
    
OutputPortBValue:
    MOV     DX, MOTOR_PORT_ADDRESS          ;output our final bits that have 
    MOV     AL, PortBValue                  ;;been set to Port B 
    OUT     DX, AL 
    ;JMP    PWMEventHandlerDone
    
PWMEventHandlerDone:
    DEC     PulseWidthCounter               ;decrement the PulseWidthCounter to
                                            ;continue counting down 
	POPA                                    ;restore all registers 
	RET	
			
PWMEventHandler	ENDP



CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'

PulseWidths 		DB	NUM_MOTORS DUP(?) ;list of pulse widths for 
                                          ;each motor; the pulse width
                                          ;determines when we should turn the 
                                          ;motor on 
PulseWidthCounter	DB	?                 ;counter for the pulse width 
                                          ;with a range of PW_COUNTER_END to 
                                          ;MAX_PULSE
MotorSpeed 		    DW	?                 ;speed of the RoboTrike; limited by 
                                          ;MIN_SPEED and MAX_SPEED with 
                                          ;MIN_SPEED meaning the RoboTrike is
                                          ;stopped
MotorAngle		    DW	?                 ;angle of the RoboTrike; limited by 
                                          ;MIN_ANGLE and MAX_ANGLE with an angle
                                          ;of MIN_ANGLE meaning straight ahead 
                                          ;relative to the RoboTrike orientation 
                                          ;and angles are measured clockwise 
LaserStatus 		DW	?                 ;status of the laser with zero values
                                          ;meaning the laser is off and non-zero 
                                          ;values meaning the laser is on 
PortBValue		    DB	?	              ;bits we need to output to Port B to 
                                          ;set the motors on/off, forward/
                                          ;backward and the laser on/off 


DATA    ENDS



        END       
