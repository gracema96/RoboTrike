;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  Motors.INC                                ;
;                          Include for Motor Functions                       ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains the constants used in the motors file.  
;
; Revision History:
;     11/13/15	Yuan Ma     initial revision
;     11/14/15  Yuan Ma     added comments 
;     11/14/15  Yuan Ma     added table definitions 
;
;
; Constant definitions for motor functions:
;
 
EXTRA_SIGN_BITS EQU     2       ;Multiplying three Q(0.15) numbers together
                                ;produces a Q(0.45) number with three sign
                                ;bits so we need to get rid of the extra ones 
SHIFT_BIT       EQU     1       ;amount of bits to shift by to get the correct
                                ;number of bits 

; Speed Constants: 
REST_SPEED	EQU	0       ;speed of the motor when stopped
MIN_SPEED	EQU	REST_SPEED  ;the minimum speed of the motors, the same 
                                    ;speed as when the RoboTrike is stopped 
MAX_SPEED	EQU	65534       ;maximum speed at which the RoboTrike can run 
IGNORE_SPEED	EQU	65535       ;indicates the speed of the RoboTrike should 
                                    ;not be changed 

; Angle Constants:              ;all angles are in degrees and measured 
                                ;clockwise from MIN_ANGLE 
MIN_ANGLE	EQU	0       ;indicates straight ahead relative to the 
                                ;RoboTrike's orientation 
MAX_ANGLE       EQU	360     ;the maximum range of the RoboTrike's angle 
IGNORE_ANGLE	EQU	-32768  ;indicates the current direction of travel of 
                                ;the RoboTrike should not be changed 

; Laser Constants:
LASER_OFF	EQU	0       ;represents the laser should be turned off  

; Motor Defintions:  
NUM_MOTORS	EQU	3       ;number of motors in total 
FIRST_MOTOR	EQU	0       ;index of the first motor
SECOND_MOTOR    EQU     1       ;index of the second motor
THIRD_MOTOR     EQU     2       ;index of the third motor 
BACKWARD_DIRECTION  EQU 1       ;bit will be set to indicate motor is 
                                ;moving in the reverse direction 
FORWARD_DIRECTION   EQU 0       ;bit will be reset to indicate motor is 
                                ;moving in the forward direction 
MOTOR_PORT_ADDRESS  EQU	181H            ;the address of port B 

; PWM Constants: 
STOP_PULSE	EQU	0       ;pulse width to stop a motor
PW_COUNTER_END  EQU	0       ;value we reach once we have finished
                                ;counting down
MAX_PULSE	EQU	127     ;maximum pulse width, the counter is 
                                ;reset if we have counted up to an 
                                ;entire pulse width 

; Port B Value Defintions: 
SET_LASER_ON    EQU	10000000B   ;set first bit to turn the laser on in
                                    ;output to Parallel Port B; will be or-ed 
                                    ;so the other bits are not changed 
SET_LASER_OFF   EQU     01111111B   ;reset first bit to turn the laser off in 
                                    ;output to Parallel Port B; will be and-ed 
                                    ;so the other bits are not changed 
LASER_MOTORS_OFF EQU    00000000B   ;turns the lasers and motors off
                                    ;0------- turns the laser off
                                    ;------0- turns motor 0 off
                                    ;----0--- turns motor 1 off 
                                    ;--0----- turns motor 2 off 
                                    ;-0-0-0-0 unused 
MOTORS_OFF      EQU     10000000B   ;turns the motors off, but doesn't change
                                    ;the laser bit 
                                    ;1------- the laser is not changed 
                                    ;------0- turns motor 0 off
                                    ;----0--- turns motor 1 off 
                                    ;--0----- turns motor 2 off 
                                    ;-0-0-0-0 unused 
 
;ForceX_Table Definitions: 
MOTOR0_X        EQU     07FFFH      ;Motor 0 x-component vector
MOTOR1_X        EQU     0C000H      ;Motor 1 x-component vector
MOTOR2_X        EQU     0C000H      ;Motor 2 x-component vector 

;ForceY_Table Defintions:  
MOTOR0_Y        EQU     00000H	    ;Motor 0 y-component vector 
MOTOR1_Y        EQU     09127H	    ;Motor 1 y-component vector 
MOTOR2_Y        EQU     06ED9H	    ;Motor 2 y-component vector 
  
;Motor_Bit_Table Defintions: 
MOTOR0_FORWARD  EQU     00000010B   ;Motor 0 on and moving forward 
                                    ;000000-- unused for motor 0
                                    ;------1- turns motor 0 on 
                                    ;-------0 sets motor 0 direction forward 
MOTOR0_BACKWARD EQU     00000011B   ;Motor 0 on and moving backward
                                    ;000000-- unused for motor 0 
                                    ;------1- turns motor 0 on 
                                    ;-------1 sets motor 0 direction backward
MOTOR1_FORWARD EQU      00001000B   ;Motor 1 on and moving forward
                                    ;0000--00 unused for motor 1 
                                    ;----1--- turns motor 1 on 
                                    ;-----0-- sets motor 1 direction forward 
MOTOR1_BACKWARD EQU     00001100B   ;Motor 1 on and moving backward 
                                    ;0000--00 unused for motor 1 
                                    ;----1--- turns motor 1 on 
                                    ;-----1-- sets motor 1 direction backward  
MOTOR2_FORWARD  EQU     00100000B   ;Motor 2 on and moving forward
                                    ;00--0000 unused for motor 2 
                                    ;--1----- turns motor 2 on 
                                    ;---0---- sets motor 2 direction forward 
MOTOR2_BACKWARD EQU     00110000B   ;Motor 2 on and moving backward 
                                    ;00--0000 unused for motor 2 
                                    ;--1----- turns motor 2 on 
                                    ;---1---- sets motor 2 direction backward  
