        NAME    MTRMAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    MTRMAIN                                 ;
;                             MOTOR BOARD Main Loop                          ;
;                                   EE/CS 51                                 ;
;                                   Yuan Ma                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;
; Input:
;
; Output:
;
; User Interface: 
;
; Error Handling: 
;
; Algorithms:
;
; Data Structures:
;
; Limitations: 
;
; Revision History:
;     11/30/15  Yuan Ma     wrote outline
;     12/8/15   Yuan Ma     wrote main loop 
;

; local include files 
$INCLUDE(events.inc) 
$INCLUDE(general.inc)       
$INCLUDE(remote.inc)

CGROUP  GROUP   CODE
DGROUP  GROUP   STACK, DATA 

CODE    SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

    ;general
    EXTRN InitCS:NEAR
    EXTRN ClrIRQVectors:NEAR
    ;EventQueue
    EXTRN InitEventQueue:NEAR
    EXTRN DequeueEvent:NEAR
    EXTRN EventQueueEmpty:NEAR
    ;critical error
    EXTRN InitCriticalError:NEAR
    EXTRN GetCriticalError:NEAR
    ;timer1 
    EXTRN InitTimer1:NEAR
    EXTRN InstallTimer1Handler:NEAR
    ;motor
    EXTRN InitMotorLaser:NEAR
    EXTRN InitParallel:NEAR
    EXTRN GetMotorSpeed:NEAR
    EXTRN GetMotorDirection:NEAR 
    EXTRN GetLaser:NEAR 
    ;int2 
    EXTRN InitINT2:NEAR
    EXTRN InstallINT2Handler:NEAR 
    ;serial
    EXTRN InitSerial:NEAR
    EXTRN SerialPutString:NEAR 
    ;parser
    EXTRN InitParseSerial:NEAR
    EXTRN ParseSerialChar:NEAR 
    EXTRN GetParseSuccessFlag:NEAR 
    ;convert
    EXTRN Hex2String:NEAR
    EXTRN Dec2String:NEAR 
    

; MotorMain   
;
; Description:      This function is the main loop for the RoboTrike 
;                   motor board. This function initializes the stack
;                   pointer and data segment and then all the necessary
;                   functions necessary for the chip select, motor, 
;                   motor timer, interrupts, serial, parser, and event
;                   queue. The function continuously loops and dequeues 
;                   events from the event queue unless the event queue is 
;                   empty and handle the events. For error events, the function
;                   will display an error string with the LSR value of the 
;                   error. For receive data events, the function will parse the 
;                   entire command (since data received is a motor command) 
;                   and then display the status of which command was just parsed. 
;
; Operation:        This function will first initialize the stack pointer
;                   and data segment and then call all the initialization 
;                   functions necessary. The function will loop continuously
;                   and indicate critical error if there is one. Otherwsise it
;                   will dequeue events while checking if EventQueue is 
;                   empty. The function will check the event type if a valid 
;                   event is received. For serial error events, the function 
;                   will display the motor serial error symbols followed by 
;                   the LSR value of the error. For commands, the function 
;                   will parse the entire command and then send the final 
;                   status of whichever command was parsed by storing the 
;                   status in status_buffer. 
;
; Input:            Keypad 
;
; Output:           Motors 
;
; Error Handling:   None    
;
; Algorithms:       None
;
; Data Structures:  None 
;
; Limitations:      None        
;
; Author: Yuan Ma 
; Last Modified: 12/12/15 
;

START:

MAIN:
    MOV     AX, DGROUP      ;initialize the stack pointer 
    MOV     SS, AX
    MOV     SP, OFFSET(DGROUP:TopOfStack) 

    MOV     AX, DGROUP      ;initialize data segment 
    MOV     DS, AX 

    CALL    InitCS
    CALL    ClrIRQVectors 
    ;JMP    ResetMotorMain

ResetMotorMain:
    CLI
    CALL    InitTimer1
    CALL    InstallTimer1Handler
    CALL    InitINT2
    CALL    InstallINT2Handler 
    CALL    InitSerial
    CALL    InitParseSerial
    CALL    InitEventQueue
    CALL    InitCriticalError
    CALL    InitParallel
    CALL    InitMotorLaser 
    STI 
    ;JMP    MotorMainLoop

MotorMainLoop:
    CALL    GetCriticalError    ;check for critical error 
    CMP     AX, RESET_FLAG      ;see it there is no critical error 
    JE      MotorHandleEvents   ;if no error, handle events 
    ;JNE    HandleCriticalError ;if critical error, handle it 

HandleCriticalError: 
    JMP     ResetMotorMain      ;reset system and start looping again 

MotorHandleEvents:
    CALL    EventQueueEmpty     ;check if EventQueue is empty
    JZ      MotorMainLoop       ;zero flag set means it’s empty and there’s
                                ;nothing to dequeue
    CALL    DequeueEvent        ;dequeue an event from EventQueue
    ;JMP    CheckMotorEventType ;check what kind of event it is 

CheckMotorEventType:
    XOR     BX, BX              ;clear BX just in case
    MOV     BL, AH              ;move event type to BL 
    CMP     BL, RX_EVENT        ;valid receive data from serial event?
    JE      HandleReceivedData  ;if so, handle it 
    
    CMP     BL, ERROR_EVENT         ;valid serial error event?
    JE      HandleMotorSerialError   ;if so, handle it 
    
    JNE     MotorMainLoop       ;if we don’t have a valid event, loop back
                                ;to check next event

HandleMotorSerialError:
    MOV     motor_error_buffer[0], MOTOR_SYMBOL
    MOV     motor_error_buffer[1], MOTOR_SYMBOL     ;indicate error is on motor side 
    MOV     motor_error_buffer[2], ERROR_SYMBOL     ;indicate serial error 
    MOV     AH, 0               
    MOV     SI, OFFSET(motor_error_buffer+3)        ;store error after the symbols      
    CALL    Hex2String                              ;call Hex2String to store LSR 
                                                    ;value 
    MOV     motor_error_buffer[DISPLAY_BUF_LEN], ASCII_NULL ;write the null
                                                            ;terminator as last 
                                                            ;digit 
    MOV     CX, DS
    MOV     ES, CX                                  ;set ES = DS since in data
                                                    ;segment 
    MOV     SI, OFFSET(motor_error_buffer)          ;set up SI to start address 
                                                    ;of motor_error_buffer 
    CALL    SerialPutString                         ;send over to remote side 
    JMP     MotorMainLoop  
    
HandleReceivedData:
    CALL    ParseSerialChar         ;event value (character to parse) already in AL 
    CMP     AX, RESET_FLAG           ;check to see if there was a parsing error
    JE      CheckStatusToSend     ;no parsing error, send current status string 
    ;JNE    SendParsingErrorString  ;if parsing error string, send string 

SendParsingErrorString:
    MOV     CX, CS
    MOV     ES, CX          ;set up ES = CS since string in code segment
    MOV     SI, OFFSET(ParseErrorString)
    CALL    SerialPutString     ;pass parse error string to remote board 
    JMP     MotorMainLoop       ;start looping again    

CheckStatusToSend: 
    XOR     AX, AX              ;clear previous value of AX 
    CALL    GetParseSuccessFlag ;check what we command was parsed 
    MOV     BX, AX 
    SHL     BX, TABLE_WORD_INDEXED         
    JMP     CS:StatusDisplayJumpTable[BX]
    
SendLaserStatus:
    XOR     AX, AX                      ;clear AX 
    CALL    GetLaser                    ;return laser status in AX 
    CMP     AX, SET_FLAG                ;check if laser is on 
    JE      SendLaserOnStatus
    ;JNE    SendLaserOffStatus
    
SendLaserOffStatus:
    MOV     SI, OFFSET(LaserOffString)  ;send the LaserOffString 
    JMP     SendLaserString 

SendLaserOnStatus: 
    MOV     SI, OFFSET(LaserOnString)   ;send the LaserOnString 
    JMP     SendLaserString 

SendLaserString:
    MOV     CX, CS
    MOV     ES, CX                      ;set ES = CS since in code segment 
    CALL    SerialPutString             ;send over to remote side 
    JMP     MotorMainLoop
 
SendDirectionStatus:
    MOV     status_buffer[0], DIRECTION_SYMBOL  ;first indicate what status 
    MOV     SI, OFFSET(status_buffer+1)     
    XOR     AX, AX                          ;clear AX 
    CALL    GetMotorDirection               ;return motor direction 
    CALL    Dec2String                      ;direction stored in status_buffer
    JMP     SendStatusString
    
SendSpeedStatus:
    MOV     status_buffer[0], SPEED_SYMBOL ;first indicate what status          
    MOV     SI, OFFSET(status_buffer+1)
    XOR     AX, AX                          ;clear AX 
    CALL    GetMotorSpeed                   ;return speed in AX 
    CALL    Hex2String                      ;speed stored in status_buffer       
    JMP     SendStatusString
    
SendStatusString:
    MOV     status_buffer[DISPLAY_LEN], ASCII_NULL
    MOV     CX, DS
    MOV     ES, CX 
    MOV     SI, OFFSET(status_buffer)
    CALL    SerialPutString 
    JMP     MotorMainLoop 

;StatusDisplayJumpTable

StatusDisplayJumpTable  LABEL   WORD
    DW      MotorMainLoop
    DW      SendLaserStatus
    DW      SendDirectionStatus
    DW      SendSpeedStatus
    
; CriticalErrorString   
CriticalErrorString LABEL   BYTE
    DB  'CriT Err', ASCII_NULL 


;ParseErrorString 
ParseErrorString    LABEL   BYTE
    DB  'PArs Err', ASCII_NULL

;LaserOffString     
LaserOffString      LABEL   BYTE
    DB  'LASErOFF', ASCII_NULL

;LaserOnString
LaserOnString       LABEL   BYTE
    DB  'LASEr On', ASCII_NULL


 

CODE    ENDS




; the data segment

DATA    SEGMENT PUBLIC 'DATA'

motor_error_buffer    DB  DISPLAY_BUF_LEN DUP(?) 
status_buffer         DB  DISPLAY_BUF_LEN DUP(?) 
DATA    ENDS



; the stack

STACK   SEGMENT STACK   'STACK'
        
        DB  80 DUP ('STACK')    ;240 words

TopOfStack  LABEL   WORD

STACK   ENDS



        END     START
