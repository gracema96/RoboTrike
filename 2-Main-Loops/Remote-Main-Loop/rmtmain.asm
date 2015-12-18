        NAME    RMTMAIN         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   RMTMAIN                                  ;
;                             REMOTE BOARD Main Loop  			     ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;	
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program tests RoboTrike remote board. The program 
;                   first initializes the stack pointer, data segment,
;                   chip select logic, and then calls another function to 
;                   initialize the timer, interrupts, display, keypad, serial,
;                   and eventqueue. The function will continuously loop and 
;                   check for critical errors. If there is a critical error, 
;                   the entire system is reset. Otherwise, the function will 
;                   handle the events by calling functions to take care of the 
;                   separate event types. 
;
; Input:            Strings to be displayed are inputted
;                   through the serial channel and a 4x4 keypad is used to 
;                   input different commands. 
;
; Output:           Commands from the keypad are outputted on the serial 
;                   channel. Strings transmitted over the serial channel are 
;                   outputted on the LED displays. 
; 
; User Interface:   The user can press buttons an a 4x4 keypad array and 
;                   observe the commands of each button through the serial
;                   channel (on RealTerm). The commands are all COMMAND_LEN 
;                   long and terminated with a carriage return. These are 
;                   the commands of each key:
;                    ____________ ____________ ____________ ____________
;                   |Left 135    |Right 135   |1/4 Speed   |1/2 Speed   |
;                   |____________|____________|____________|____________| 
;                   |Left 45     |Right 45    |Fire Laser  |Laser Off   |
;                   |____________|____________|____________|____________| 
;                   |Left 10     |Right 10    |Accelerate  |Decelerate  |
;                   |____________|____________|____________|____________| 
;                   |Turn Left   |Turn Right  |Reverse     |Stop        |
;                   |____________|____________|____________|____________|
;                   The user may also send strings through the serial channel 
;                   and observe the string being displayed on the 7-segment LED
;                   If there is an error, an error string will also be 
;                   displayed.
; 
; Error Handling:   If there is a critical error, the program will reset and 
;                   display an error string. If there is a baud, parity, 
;                   framing, or overrun error, a remote error string will be 
;                   displayed. 
; 
; Algorithms:       None 
;
; Data Strucutres:  None 
; 
; Limitations:      None 
; 
; File Description: 
;	This file contains the RoboTrike remote board functions and their 
;	functional specifications: RemoteMain, ResetRemoteMain, 
;	KeypressHandler, ReceiveSerialDataHandler, and SerialErrorHandler. 
;	Together, these functions handle the keypad, display, and serial 
;	user interface of the RoboTrike and report any errors that occur. 
;
; Table of Contents:
;	RemoteMain:		  main loop for the RoboTrike remote board 
;	ResetRemoteMain:	  resets all the components of the RoboTrike
;				  remote board main loop 
;	KeypressHandler:	  handles different keypresses for the RoboTrike
;				  remote board 
;	ReceiveSerialDataHandler: handles data transmitted over the serial 
;				  channel for the RoboTrike remote board
;	SerialErrorHandler:	  handles serial errors for the RoboTrike 
;                                 remote board
;
; Tables:
; RemoteEventHandlerTable:  	table of event handler functions 
; KeypressCommandTable:     	table of command to send over serial for 
;                           	each keypress 
; 
; Revision History:
;     11/30/15  Yuan Ma		wrote outline
;     12/4/15	Yuan Ma		wrote functions
;     12/5/15	Yuan Ma		wrote keypress handler and table 
;     12/6/15   Yuan Ma   	debugged code and updated comments 
;


;local include files 
$INCLUDE(remote.inc)        ;constants for remote functions 
$INCLUDE(events.inc)        ;events 
$INCLUDE(general.inc)       ;general constants 


CGROUP  GROUP   CODE
DGROUP  GROUP   STACK, DATA 

CODE	SEGMENT PUBLIC 'CODE'

	ASSUME  CS:CGROUP, DS:DGROUP

  	;functions needed to initialize the chip select, interrupts, event queue, 
  	;keypad, display, serial, timer0, and a conversion function 
	;general functions 
	EXTRN InitCS:NEAR
	EXTRN ClrIRQVectors:NEAR 
	;EventQueue functions 
	EXTRN InitEventQueue:NEAR
	EXTRN DequeueEvent:NEAR 
	EXTRN EventQueueEmpty:NEAR
	;critical error functions 
	EXTRN InitCriticalError:NEAR
	EXTRN GetCriticalError:NEAR 
	;keypad functions 
	EXTRN KeypadScanInit:NEAR 
	;display functions 
	EXTRN InitDisplay:NEAR
	EXTRN Display:NEAR  
	;serial functions 
	EXTRN InitSerial:NEAR 
	EXTRN SerialPutString:NEAR 
	;timer0 functions 
	EXTRN InitTimer0:NEAR
	EXTRN InstallTimer0Handler:NEAR
	;int2 functions 
	EXTRN InitINT2:NEAR
	EXTRN InstallINT2Handler:NEAR 
    	;converts functions 
    	EXTRN Hex2String:NEAR 


; RemoteMain   
;		
; Description:      	This function is the main loop for the RoboTrike 
;		    	remote board. This function initializes the stack
;	 	    	pointer, data segment, chip select logic, and then calls
;                   	another function to initialize the timer, interrupts, 
;                   	display, keypad, serial, and eventqueue. The function 
;                   	continuously loops and checks to make sure there are no 
;                   	critical errors (occurs when the EventQueue is full). If 
;                   	there is a critical error, the function will just reset 
;                   	everything. If there isn't a critical error, the function 
;                   	will dequeue events from the event queue while the event 
;                   	queue isn't empty. After determining the type of event 
;                   	that has been dequeued, the function will handle each type 
;                   	of event by calling the appropriate handler for the event. 
;
; Operation:		This function will first initialize the stack pointer
;		    	and data segment, chip select and then call function 
;                   	ResetRemoteMain in order to inialize all other 
;               	necessary functions. The function will loop continuously
;                   	and check if there is a critical error. If there is a 
;                   	critical error, the function will reset before looping again. 
;                   	If there isn't a critical error, the function will check if 
;                   	the EventQueue is empty and dequeue an event if it isn't 
;                   	empty. If the EventQueue is empty, the function will just 
;               	start looping again. After dequeueing an event, the funciton 
;                   	will check to see if it is a KEY_EVENT (a key has been pressed), 
;                   	RX_EVENT (serial data has been received), or ERROR_EVENT 
;                   	(there was some sort of serial error). If the data dequeued 
;                   	is none of these, the function will start looping again. After
;                   	determining the appropriate event, the function will use 
;                   	a table to call the appropriate event handler and deal 
;                   	with the event. 
;
; Input:		A 4x4 keypad array and the serial channel 
;
; Output:		Serial channel and LED display 
;
; Error Handling:	If a critical error occurs, the main loop is reset.  	
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 12/6/15
;

START:


MAIN: 
	MOV	AX, DGROUP		    ;initialize the stack pointer 
	MOV	SS, AX
	MOV	SP, OFFSET(DGROUP:TopOfStack)

	MOV	AX, DGROUP		    ;initialize data segment 
	MOV	DS, AX 

	CALL	InitCS			    ;initialize the 80188 chip selects
                                	    ;assume LCS and UCS already set up 
	CALL	ClrIRQVectors		    ;clear (initialize) interrupt vector table 
	CALL	ResetRemoteMain		    ;call function to initialize the rest of 
                                	    ;the necessary functions 
	;JMP	RemoteMainLoop

RemoteMainLoop:
	CALL	GetCriticalError	    ;obtain the critical error value in AX 
	CMP	AX, RESET_FLAG  	    ;check to see if there is a critical error 
	JE	RemoteHandleEvents 	    ;if there is no critical error, we can move
                                	    ;on to dequeueing an event from the EventQueue
	;JNE	HandleCriticalError	    ;if there is a critical error, handle it 

HandleCriticalError:
	CALL	ResetRemoteMain		    ;call function ResetRemoteMain to reset the 
                                	    ;main loop since there was a critical error
                                	    ;this resets the event queue 
	JMP	RemoteMainLoop	            ;start looping again 

RemoteHandleEvents: 
	CALL	EventQueueEmpty		    ;call function EventQueueEmpty to check 
                                	    ;if EventQueue is empty
	JZ	RemoteMainLoop		    ;if the zero flag is set, this means the 
                                	    ;EventQueue is empty and there are no events
                                	    ;to dequeue 
	CALL	DequeueEvent		    ;call function DequeueEvent to dequeue an 
                                   	    ;event from the EventQueue 
	;JMP	CheckRemoteEventType 	    ;check what type of event it is 

CheckRemoteEventType:
	XOR	BX, BX			    ;clear BX just in case there was anything there 
	MOV	BL, AH 			    ;move event value to BL for table indexing 
                                	    ;later 
                                	    
                                	    ;now check if we have a valid event
	CMP	BL, KEY_EVENT 	            ;check if we have a valid keypress event 	
	JE	HandleRemoteEvent   	    ;if so, we will handle it 

	CMP	BL, RX_EVENT 	    	    ;check if we have a valid data received from 
                                	    ;serial event
        JE	HandleRemoteEvent   	    ;if so, we will handle it 
	
	CMP	BL, ERROR_EVENT 	    ;check if we have a valid serial error event 
	JE	HandleRemoteEvent   	    ;if so, we will handle it 

	JNE	RemoteMainLoop		    ;if we don’t have a valid event, loop back

HandleRemoteEvent:
	SHL	BX, TABLE_WORD_INDEXED      ;remote event handler table is
                                            ;indexed as words so we must 
                                            ;shift BX 
	CALL 	CS:RemoteEventHandlerTable[BX]  ;call the proper function as 
                                            ;indexed in the table to 
                                            ;handle the event appropriately 
	JMP	RemoteMainLoop		    ;finished handling the event, so 
                                            ;loop back 
	 


;RemoteEventHandlerTable 
; 
; Description: 	The is the table of event handler functions that will be 
;               called in the main loop to handle different types of functions.
;               There are functions to handle keypress events, receiving 
;               data from serial events, and serial error events. 
; 
; Notes:	READ ONLY tables should always be in the code segment 
;               so that in a standalone system it will be located in 
;               the ROM with the code.
;
; Author: Yuan Ma
; Last Modified: 12/6/15

RemoteEventHandlerTable	LABEL	WORD
	DW	NO_EVENT_HANDLER	;no event handler, but used for 
                                        ;table indexing 
	DW	KeypressHandler		;handle keypress event (KEY_EVENT) 
	DW	ReceiveSerialDataHandler    ;handle receive data from serial event 
                                            ;(RX_EVENT) 
	DW	SerialErrorHandler      ;handle serial error event (ERROR_EVENT) 


; ResetRemoteMain 
;
; Description:          This function just resets components of the RoboTrike  
;		        remote board in order to initialize them. The function will
;                       initialize timer0, interrupts, the display, the display
;                       muxing, the keypad, the serial, the eventqueue, the
;                       critical error flag, the display_buffer_index, and the 
;                       display_error flag. This function is also called in the 
;                       case of a critical error. Interrupts are turned off while 
;                       resetting.  	
;
; Operation:	        The function will first turn interrupts off while 
;                       initializing functions. Then if will calls the the 
;                       functions necessary to initialize timer0, int2, 
;                       install appropriate handlers, display, display
;                       muxing, keypad, serial, EventQueue, and critical error flag. 
;                       It will also set the display_buffer_index to 0 to 
;                       represent the first character and indicate that there 
;                       are no errors being displayed. Finally interrupts are 
;                       renabled at the end. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	Shared variables associated with each initialization function
;			described in that function’s functional specification  
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
; Registers Changed:   Registers changed associated with each initialization 
;                      function described in that function’s functional specification 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 12/6/15
;

ResetRemoteMain      	PROC        NEAR

    CLI                             ;turns interrupts off while initializing  	
    CALL    InitTimer0 		    ;initialize the keypad and display timer
                                    ;and allow interrupts 	
    CALL    InstallTimer0Handler    ;install the timer 0 event handler. Must 
                                    ;install handlers before allowing the 
                                    ;hardware to interrupt 
    CALL    InitINT2		    ;initialize the INT2 interrupts 
    CALL    InstallINT2Handler 	    ;install the INT2 event handler. Must install
                                    ;handlers before allowing the hardware to 
                                    ;interrupt 
    CALL    InitDisplay		    ;initializes the display 
    CALL    KeypadScanInit	    ;initializes the keypad    
    CALL    InitSerial		    ;initializes the serial I/O
    CALL    InitEventQueue	    ;initializes the event queue 
    CALL    InitCriticalError	    ;initialize critical error flag  
    MOV	    display_buffer_index, 0 ;start display string index to first
                                    ;character of display buffer 
    MOV     display_error, RESET_FLAG   ;initially not displaying any errors 
    STI                             ;enable the interrupts 
    RET				

ResetRemoteMain		ENDP

; KeypressHandler 
;
; Description:		This function will handle the keypress events and 
;			determine the appropriate command associated with each key
;			and then indirecty transmit the command string over the 
;                       serial channel. Multiple keys pressed at one time will 
;                       not be handled. Commands corresponding to each key will 
;                       be defined in the KeypressCommandTable. This function is 
;                       called whenever a key event is dequeued in AH from the 
;                       EventQueue.  
;
; Operation:		The function will calculate the index for each key by 
;                       multiplying the event value by COMMAND_LEN. The function 
;                       will find the appropriate command for each keypress 
;                       using table lookup. The function will then check to see
;                       if an error is being displayed. If so, the function will
;                       return and nothing will be transmitted over the serial 
;                       channel. If an error is not being displayed, the function
;                       SerialPutString will be called to store the command 
;                       function to TxQueue which will then transmit the command 
;                       over the serial channel. 
;
; Arguments:		AL - key value of the keypress to handle 
;                       key values were previously defined in the keypad file 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	display_error - nonzero value indicates an error is being
;                                       displayed, a zero value indicates no error
;                                       is being displayed (read from) 
;
; Global Variables:	None 
;
; Input:		A 4x4 keypad array 
;
; Output:		Command message to the serial channel 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:	None  
;
; Limitations:		Muliple keys pressed at one time are not handled.   	
;
; Author: Yuan Ma 
; Last Modified: 12/6/15
;
;

KeypressHandler      	PROC        NEAR


    PUSHA				;save registers 
    
CheckDisplayError: 
    CMP     display_error, SET_FLAG     ;check to see if display_error 
                                        ;flag indicates we are displaying 
                                        ;an error 
    JE      KeypressHandlerEnd          ;if we are, we should not output
                                        ;a command string, so just return 
    ;JNE    GetKeypressTableAddress     ;if we are, we need to get the 
                                        ;address of the command to output 
                                            
GetKeypressTableAddress: 
    MOV	    AH, 0 			;we now only care about the key value
                                        ;since we have already correctly determined
                                        ;this as a key event not  
    MOV     BH, 0                       ;clear BH in case there is a value there 
    MOV	    BL, COMMAND_LEN 	        ;set up BL with the COMMAND_LEN so that 
    MUL	    BX			        ;when multiplied by the key value, we will 
                                        ;obtain the appropriate index of the 
                                        ;corresponding key 
    MOV	    BX, CS                      ;must set ES = CS since the 
    MOV	    ES, BX			 ;KeypressCommandTable is in the code segment 
    MOV	    SI, OFFSET(KeypressCommandTable) ;set up SI with the starting 
                                             ;address of the KeypressCommandTable
    ADD	    SI, AX                      ;add the index to the start address of the 
                                        ;KeyCommandTable so we get the index of 
                                        ;the corresponding key command 
    ;JMP    SendKeypressOverSerial      ;we can now send the command over the
                                        ;serial channel 
    
SendKeypressOverSerial: 
    CALL    SerialPutString		;call function SerialPutString to store 
                                        ;the command in the TxQueue which will 
                                        ;be transmitted over the serial channel 
    ;JMP    KeypressHandlerEnd          ;done handling the keypress 
    
KeypressHandlerEnd: 
    POPA				;restore registers 	
    RET				

KeypressHandler 	ENDP


; KeypressCommandTable
;
; Description: 	    This table gives the command corresponding to each key. 
;                   The commands are all COMMAND_LEN long with a null terminator. 
;                   Blank strings are used as place holders, so the key values 
;                   can be properly indexed in the table. 
;		    The keypad is arranged with the key values like so: 
;		    0E	0d	0b	07
;		    1E	1d	1b	17
;		    2E	2d	2b	27
;		    3E	3d	3b	37
;
;		    Corresponding commands for each key: 
;		    0E - turn 135 degrees left    
;		    0d - turn 135 degrees right   
;		    0b - fourth speed    
;		    07 - half speed   
;	 	    1E - turn 45 degrees left 
;		    1d - turn 45 degrees right 
;		    1b - fire laser 
;	            17 - laser off 
;		    2E - turn 10 degrees left 
;		    2d - turn 10 degrees right 
;		    2b - accelerate speed 500
;		    27 - decelerate speed 500 
;		    3E - turn left 
;		    3d - turn right 
;		    3b - reverse robotrike 
;		    37 - stop robotrike
; 
; Notes:	    READ ONLY tables should always be in the code segment so 
;                   that in a standalone system it will be located in the 
;                   ROM with the code. 
; 
; Author: Yuan Ma
; Last Modified: 12/6/15 


KeypressCommandTable 	LABEL	BYTE 
	DB '       ', ASCII_NULL	;0
	DB '       ', ASCII_NULL	;1
	DB '       ', ASCII_NULL	;2
	DB '       ', ASCII_NULL	;3
	DB '       ', ASCII_NULL	;4
	DB '       ', ASCII_NULL	;5
	DB '       ', ASCII_NULL	;6
	DB 'S32767 ', ASCII_NULL	;7 half speed 	
	DB '       ', ASCII_NULL	;8
	DB '       ', ASCII_NULL	;9
	DB '       ', ASCII_NULL	;10
	DB 'S16384 ', ASCII_NULL 	;11 fourth speed 
	DB '       ', ASCII_NULL	;12
	DB 'd-135  ', ASCII_NULL        ;13 turn 135 right 
	DB 'd+135  ', ASCII_NULL 	;14 turn 135 left 	
	DB '       ', ASCII_NULL	;15
	DB '       ', ASCII_NULL	;16
	DB '       ', ASCII_NULL	;17
	DB '       ', ASCII_NULL	;18
	DB '       ', ASCII_NULL	;19
	DB '       ', ASCII_NULL	;20
	DB '       ', ASCII_NULL	;21
	DB '       ', ASCII_NULL	;22
	DB 'O      ', ASCII_NULL        ;23	turn laser off 
	DB '       ', ASCII_NULL	;24
	DB '       ', ASCII_NULL	;25
	DB '       ', ASCII_NULL	;26
	DB 'F      ', ASCII_NULL	;27	fire laser 
	DB '       ', ASCII_NULL	;28
	DB 'd-45   ', ASCII_NULL	;29	turn 45 right 
	DB 'd+45   ', ASCII_NULL	;30	turn 45 left 
	DB '       ', ASCII_NULL	;31
	DB '       ', ASCII_NULL	;32
	DB '       ', ASCII_NULL	;33
	DB '       ', ASCII_NULL	;34
	DB '       ', ASCII_NULL	;35
	DB '       ', ASCII_NULL	;36
	DB '       ', ASCII_NULL	;37
	DB '       ', ASCII_NULL	;38
	DB 'V-500  ', ASCII_NULL	;39	decelerate by 500 
	DB '       ', ASCII_NULL	;40
	DB '       ', ASCII_NULL	;41
	DB '       ', ASCII_NULL	;42
	DB 'V+500  ', ASCII_NULL	;43	accelerate by 500 
	DB '       ', ASCII_NULL	;44
	DB 'd-10   ', ASCII_NULL	;45	turn slightly right 
	DB 'd+10   ', ASCII_NULL	;46	turn slightly left 
	DB '       ', ASCII_NULL	;47
	DB '       ', ASCII_NULL	;48
	DB '       ', ASCII_NULL	;49
	DB '       ', ASCII_NULL	;50
	DB '       ', ASCII_NULL	;51
	DB '       ', ASCII_NULL	;52
	DB '       ', ASCII_NULL	;53
	DB '       ', ASCII_NULL	;54
	DB 'S0     ', ASCII_NULL	;55	stop RoboTrike  
	DB '       ', ASCII_NULL	;56
	DB '       ', ASCII_NULL	;57
	DB '       ', ASCII_NULL	;58
	DB 'd180   ', ASCII_NULL	;59	reverse direction  
	DB '       ', ASCII_NULL	;60
	DB 'd+90   ', ASCII_NULL	;61	turn right 
	DB 'd-90   ', ASCII_NULL	;62	turn left 



; ReceiveSerialDataHandler   
;
; Description:		This function handles any serial data that is received
;                       and will display the data on the LED displays. Serial data
;                       is recognized as a string up to DISPLAY_LEN long followed 
;                       by a carriage return. The data from th serial channel is 
;                       processed one character at a time and stored in a 
;                       temporary display_buffer. If an error is being displayed,
;                       the display will reset the appropriate shared variables. 
;                       This function is called whenever a serial data received
;                       event is dequeued in AH from the EventQueue. Long strings 
;                       are truncated and will only display up to DISPLAY_LEN. 
;
; Operation:		The function will check to see if a carriage return has
;                       been sent over the serial channel. If so, the function will
;                       display the string that is stored in the display_buffer. 
;                       If not, the function will loop and store characters in the 
;                       display_buffer and check to make sure there aren't more 
;                       characters than DISPLAY_BUF_LEN. If there are more 
;                       characters than DISPLAY_BUF_LEN, the index will 
;                       automatically be set to the last display character. Before 
;                       the display_buffer can be displayed, we must check to 
;                       make sure we are not displaying an error. If we are, we 
;                       will reset the display_buffer_index to the first character
;                       and reset the display_error flag. If we are not displaying
;                       an error, then we will set the null terminator as the 
;                       last character of display_buffer and then call Display to 
;                       display the final display_buffer. After displaying the 
;                       display_buffer, the display_buffer_index is then reset to 
;                       the beginning again. 
;
; Arguments:		AL - character received over the serial channel 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	display_buffer - buffer to store characters that will 
;					 eventually be displayed (written to)
;                       display_buffer_index - index of characters in the 
;                                               display_buffer (written/read)
;                       display_error - nonzero value indicates an error is being
;                                       displayed, a zero value indicates no error
;                                       is being displayed (written/read)
;
; Global Variables:	None 
;
; Input:		Data from the serial channel 
;
; Output:		LED displays 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:    None  
;
; Limitations:		The string to be displayed can only be up to COMMAND_LEN 
;                       long. 
;
; Author: Yuan Ma 
; Last Modified: 12/6/15
;

ReceiveSerialDataHandler    	PROC        NEAR

	PUSHA			             ;save registers

CheckEOSChar:
    CMP	    AL, ASCII_EOS 		     ;check to see if character we 
                                             ;received is carriage return 
    JE	    DisplaySerialData                ;if so, we can display what we have
                                             ;in the display_buffer 
    ;JNE    StoreNextChar	             ;if not, we will store the next 
                                             ;character to display 

StoreNextChar:
    MOV	    BX, display_buffer_index	     ;get current display buffer index
    MOV	    display_buffer[BX], AL           ;save the character to the current
                                             ;index of the display_buffer 
    INC	    display_buffer_index		    ;increment to store the next character
    CMP	    display_buffer_index, DISPLAY_BUF_LEN   ;check to make sure we don't
                                                    ;have more characters than 
                                                    ;we can display 
    JL	    ReceiveDataDone 		    ;if so, we are done storing this 
                                            ;character 
    ;JNB    SetDisplayLastChar		    ;if not, we have to set index to 
                                            ;the last character

SetDisplayLastChar:
    MOV	    display_buffer_index, DISPLAY_LEN 	;set index to last display character
    JMP	    ReceiveDataDone                     ;now we are done 

DisplaySerialData:                          ;ready to display serial data 
    CMP     display_error, SET_FLAG         ;but must first check if we are 
                                            ;displaying an error 
    JNE     DisplayNoReset                  ;if not, we don't have to reset 
    ;JE     DisplayReset                    ;if so, we have to reset the display 
    
DisplayReset:
    MOV     display_buffer_index, 0         ;reset the index of display_buffer_index
                                            ;the first character 
    MOV     display_error, RESET_FLAG       ;reset the display_error flag 
                                            ;because we have dealt with it 
    ;JMP    DisplayNoReset

DisplayNoReset: 
    MOV	    BX, display_buffer_index        ;get the current display_buffer_index
    MOV	    display_buffer[BX], ASCII_NULL  ;send null terminator to display_buffer
                                            ;to indicate the end of the string 
    MOV	    BX, DS                          ;set ES = DS since the 
    MOV	    ES, BX 			    ;display_buffer is in the data segment  
    MOV	    SI, OFFSET(display_buffer)	    ;set up SI with the initial start 
                                            ;address of the display_buffer 
    CALL    Display			     ;call function Display to display 
                                            ;the contents of the display_buffer 
    MOV	    display_buffer_index, 0 	    ;we are done displaying the 
                                            ;display_buffer, so start the 
                                            ;display_buffer_index at the 
                                            ;beginning again for next time 
    ;JMP    ReceiveDataDone

ReceiveDataDone:
    POPA			            ;restore registers 		
    RET				

ReceiveSerialDataHandler	ENDP


; SerialErrorHandler 
;
; Description:		This function will handle serial error events by 
;                       displaying an error string if there are serial error events
;                       and doesn't allow other data to be sent over. The error 
;                       string is determined by the LSR value with the possible 
;                       errors being: parity errror, ovrrrun error, framing
;                       error, and baud rate error. This function is 
;                       called whenever a serial error event is dequeued in AH 
;                       from the EventQueue.   		
;
; Operation:		This function will first set up the first character of 
;                       the display_error_buffer as ERROR_SYMBOL to indicate an 
;                       error message. Next, the function will clear the serial 
;                       error event, since we only care about the serial error 
;                       value now. The function will set up SI as the address of the 
;                       display_error_buffer after the ERROR_SYMBOL was written 
;                       and call function Hex2String to write the LSR value as an 
;                       ASCII string to the display_error_buffer after the 
;                       ERROR_SYMBOL. Hex2String will ensure the the string is 
;                       within the appropriate display bounds and will null 
;                       terminate the string. Finally, the function Display will be
;                       called with SI set up as the start address of the 
;                       display_error_buffer to display the entire error message. 
;                       The display_error flag will be set to indicate that an
;                       error is currently being displayed so other serial data 
;                       can't be sent. 
;
; Arguments:		AL - LSR value of the serial error  
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	display_error - nonzero value indicates an error is being
;                                       displayed, a zero value indicates no error
;                                       is being displayed (written to)
;                       display_error_buffer - buffer to store the error message 
;                                              (written to)
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
; Limitations:		The function can only display the error message of the 
;                       first error that occurs. In this case, 'E00008' will be 
;                       displayed.   	
;
; Author: Yuan Ma 
; Last Modified: 12/6/15 
; 

SerialErrorHandler      PROC        NEAR

    PUSHA				         ;save registers
    MOV     display_error_buffer[0], REMOTE_SYMBOL  ;write the ERROR_SYMBOL as the 
                                                ;first character of the 
                                                ;display_error_buffer to 
                                                ;indicate an error message 
    MOV     display_error_buffer[1], ERROR_SYMBOL 
    MOV     AH, 0                               ;we now only care about the 
                                                ;error value since we have 
                                                ;already correctly determined
                                                ;this as a serial error event 
    MOV     SI, OFFSET(display_error_buffer+2)    ;set up SI with the start 
                                                ;address of the display_error_buffer
    ;INC    SI                                  ;increment to the next character
                                                ;since we wrote ERROR_SYMBOL
                                                ;as the first symbol 
    CALL    Hex2String                          ;call function Hex2String to 
                                                ;write the error value as the 
                                                ;error message after ERROR_SYMBOL
    MOV	    CX, DS                              ;set up ES = DS since the 
    MOV	    ES, CX			        ;display_error_buffer is in the 
                                                ;data segment 
    MOV	    SI, OFFSET(display_error_buffer)    ;set up SI with the start 
                                                ;address of the display_error_buffer
    CALL    Display 		                ;call function Display to 
                                                ;display the error string from
                                                ;the beginning 
    MOV     display_error, SET_FLAG             ;set the display_error flag 
                                                ;to indicate we are displaying 
                                                ;an error 
    POPA				        ;restore registers
    RET				

SerialErrorHandler	ENDP



CODE    ENDS



; the data segment 

DATA    SEGMENT PUBLIC 'DATA'

display_buffer_index	DW ?                        ;index of characters in the 
                                                    ;display_buffer 
display_buffer		DB DISPLAY_BUF_LEN DUP(?)   ;buffer to store characters 
                                                    ;that will displayed 
                                                    ;on the LED displays 
display_error           DW ?                        ;nonzero value indicates 
                                                    ;an error is being
                                                    ;displayed, a zero value 
                                                    ;indicates no error
                                                    ;is being displayed      
display_error_buffer    DB DISPLAY_BUF_LEN DUP(?)   ;buffer to store the 
                                                    ;error message that will 
                                                    ;be displayed on the LED 
                                                    ;displays 


DATA    ENDS

;the stack

STACK	SEGMENT STACK	'STACK'

		DB	80 DUP ('STACK')	;240 words

TopOfStack	LABEL	WORD

STACK 	ENDS


	END	START  

