        NAME    SERIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    SERIAL                                  ;
;                                Serial Functions                            ;
;                                   EE/CS 51				     ;
;				     Yuan Ma 				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; File Description: 
;	This file contains the serial function for the RoboTrike and their 
;	functional specifications: InitSerial, SetBaudRate, SetParity, 
;	SerialPutChar, SerialPutString, and SerialEventHandler. Together, 
;   	these functions initialize the serial I/O software, set the baud rate 
;   	and parity, and allows characters to be outputted over the serial channel. 
;
;
; Table of Contents:  
;	InitSerial:		initializes serial I/O software and shared
;				variables necessary for the serial I/O 
;	SetBaudRate: 		sets the serial baud rate 
;	SetParity: 		sets the serial parity 
;	SerialPutChar:		outputs a character over the serial channel 
;	SerialPutString:	outputs a string over the serial channel 
;	SerialEventHandler:	handles events for the serial channel using interrupts 
;
;	Tables: 
;   	BaudRateTable:     	table of baud rate divisor values needed to 
;                       	set the baud rate in SetBaudRate
;  	ParityTable:       	table of bits to set the LCR value to for each parity 
;   	InterruptJumpTable: 	jump table to handle different kinds of interrupts 
;
;
;
; Revision History:
;     11/19/15  Yuan Ma	    wrote functions and their functional 
;			    specifications and comments 
;     11/20/15  Yuan Ma     finished debugging and updated comments 
;     11/21/15  Yuan Ma     updated comments 
;     11/22/15  Yuan Ma     finished commenting 
;     12/4/15	Yuan Ma     added SerialPutString function 
;     12/6/15   Yuan Ma     commented SerialPutString 
;


;include files
$INCLUDE(serial.inc)    ;constants used for the serial functions 
$INCLUDE(events.inc)
$INCLUDE(int.inc)	;INTCltrEOI value
$INCLUDE(int2.inc)	;INT2EOI value 
$INCLUDE(queue.inc)     ;definition for the shared variable TxQueue 
$INCLUDE(general.inc)   ;some ASCII values 


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP


;external function declarations:
	EXTRN	QueueInit:NEAR      ;initializes an empty queue at a specified 
                                    ;address with byte or word sized queue elements 
	EXTRN	QueueEmpty:NEAR     ;determines whether or not a queue at a 
                                    ;specified address is empty or not; will 
                                    ;set the zero flag to indicate empty 
	EXTRN	QueueFull:NEAR      ;determines whether or not a queue at a 
                                    ;specifiec address is full or not; will set
                                    ;the zero flag to indicate full 
	EXTRN	Dequeue:NEAR        ;removes and returns the element at the 
                                    ;beginning of a queue at a specified address 
	EXTRN	Enqueue:NEAR        ;adds a passed in value to the end of a queue
                                    ;at a specified address 
	EXTRN	EnqueueEvent:NEAR   ;enqueues different kinds of events 


; BaudRateTable
;
; Description: 	Table of serial baud rate divisor values that are referenced  
;		by SetBaudRate assuming a 9.216 MHz crystal. These values 
;               will be written to the divisor latch register in order to 
;               write to the baud generator. The baud rate divisors are 
;               calculated by taking the crystal frequency / desired baud
;               rate / 16. 
;
; Notes:	 READ ONLY tables should always be in the code 
;		 segment so that in a standalone system it will be 
;		 located in the ROM with the code 
; 
; Author: Yuan Ma
; Last Modified: 11/21/15

BaudRateTable	LABEL	WORD

    ;DW Baud rate divisor 
    
	DW 	11520 	;baud rate 50
	DW	7680	;baud rate 75
	DW	3840	;baud rate 150
	DW	1920	;baud rate 300
	DW	9600	;baud rate 600
	DW	480 	;baud rate 1200
	DW	320 	;baud rate 1800
	DW	240 	;baud rate 2400
	DW	160 	;baud rate 3600
	DW	120 	;baud rate 4800 
	DW	80  	;baud rate 7200	
	DW	60  	;baud rate 9600
	DW	30  	;baud rate 19200
	DW	15  	;baud rate 38400 
		

; ParityTable
; 
; Description:	Table of bits to set the LCR value for each type of parity.
;               This value will be or-ed so we don't set the other bits so
;               that when this value is or-ed, it will not affect the other 
;               bits. 
;
; Notes:        READ ONLY tables should always be in the code 
;		segment so that in a standalone system it will be 
;		located in the ROM with the code 
;
; Author: Yuan Ma
; Last Modified: 11/21/15

ParityTable	LABEL	BYTE

    ;DB bits to set LCR value 
    
	DB	00000000B	;no parity
				;00---000 unrelated to parity
				;--0----- stick parity not set 
				;---0---- even parity select not set 
				;----0--- parity bit not enabled
	DB	00001000B	;odd parity
				;00---000 unrelated to parity
				;--0----- stick parity not set
				;---0---- even parity select not set 
				;----1--- parity bit enabled 
	DB	00011000B	;even parity
				;00---000 unrelated to parity
				;--0----- stick parity not set
				;---1---- even parity select set 
				;----1--- parity bit enabled 
	DB	00101000B	;parity bit transmitted and checked as set 
				;00---000 unrelated to parity
				;--1----- stick parity not set
				;---0---- even parity select not set 
				;----1--- parity bit enabled 
	DB	00111000B	;parity bit transmitted and checked as cleared
				;00---000 unrelated to parity
				;--1----- stick parity set
				;---1---- even parity select set 
				;----1--- parity bit enabled 
    
    
; InterruptJumpTable 
; 
; Description:	Jump table of different interrupts that tells what 
;               section of the code to jump to depending on the 
;               interrupt that has been read from the IIR address.
;
; Notes:        READ ONLY tables should always be in the code 
;		segment so that in a standalone system it will be 
;		located in the ROM with the code 
;
; Author: Yuan Ma
; Last Modified: 11/22/15

InterruptJumpTable	LABEL	WORD 

    ;DW code to jump to depending on the interrupt  
    
    DW  ModemStatusInterrupt        ;modem status interrupt
                                    ;00000--- reserved 
                                    ;-----00- priority 4
                                    ;-------0 interrupt pending  
    DW	TransmitterEmptyInterrupt   ;transmitter holding register 
                                    ;empty interrupt
                                    ;00000--- reserved 
                                    ;-----01- priority 3
                                    ;-------0 interrupt pending
    DW  ReceiveDataInterrupt        ;received data available interrupt
                                    ;00000--- reserved 
                                    ;-----10- priority 2
                                    ;-------0 interrupt pending
    DW  ErrorInterrupt              ;receiver line status interrupt 
                                    ;00000--- reserved 
                                    ;-----11- priority 1
                                    ;-------0 interrupt pending 

; InitSerial  
;
; Description:		This function initializes the shared variables 
;			necessary for the serial I/O. The baud rate 
;			divisor is set to DEFAULT_BAUD_DIVISOR, the parity is 
;                   	set to DEFAULT_PARITY, and TxQueue is initialized 
;			to be empty with byte sized elements. The user can 
;                  	different values by accessing a different index in the 
;                  	BaudRateTable and ParityTable. The serial I/O 
;			software is initialized by enabling serial 
;			interrupts. The word length is set to 8 and there is
;                  	one stop bit. 
;
; Operation:		This function writes to the registers to enable
;			the serial software. The LCR value is set to 
;                   	INIT_LCR_VAL and the IER value is set to 
;                   	INIT_IER_VAL. The function SetBaudRate is called 
;                   	to set the baud rate to the default baud rate 
;                   	using DEFAULT_BAUD_DIVISOR, the function SetParity
;                   	is called to set the default parity to DEFAULT_PARITY
;                   	and an empty queue, TxQueue, with byte sized elements 
;                   	is initialized by calling function QueueInit from HW3.
;                   	The KickStarter is also initialized to NO_KICKSTART_NEEDED.  
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	TxQueue - queue of elements to be transmitted
;				  over the serial with byte sized
;				  elements (written to) 
;			KickStarter - indicates whether we need to manually 
;                                     kickstart the transmitter empty interrupt. 
;                                     (written to) 
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		LCR value - 8 bit value to write to the line control 
;                                   register with no parities set 
;                  	IER value - 8 bit value to write to the interrupt enable
;                                   register with all of the serial interrupts 
;                                   enable 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:	AX, BX, DX, SI 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/22/15
;
 

InitSerial      	PROC        NEAR
                	PUBLIC      InitSerial 


InitLCRAddress:
	MOV	DX, LCR_ADDRESS		;set up the initial value to write to the LCR  
	MOV	AX, INIT_LCR_VAL	;(no parities are set) 
	OUT	DX, AL
	;JMP    InitIERAddress 

InitIERAddress:	
	MOV	DX, IER_ADDRESS		;set up the initial value to write to the IER 
	MOV	AX, INIT_IER_VAL    	;(all of the serial interrupts are set) 
	OUT	 DX, AL
	;JMP    InitBaudRate 

InitBaudRate:
	MOV	AX, DEFAULT_BAUD_DIVISOR ;initialize with default baud divisor 
	CALL	SetBaudRate		  ;call the function SetBaudRate to                                  
                                          ;actually set baud rate to divisor 
                                          ;latch register
	;JMP    InitParity 

InitParity:
	MOV	AX, DEFAULT_PARITY	;initialize to no parities 
	CALL	SetParity 		;call function SetParity to actually set parity
	;JMP    InitTxQueue 

InitTxQueue:
	MOV	SI, OFFSET(TxQueue)	;set SI to address to start TxQueue
	MOV	BL, QUEUE_BYTE_SIZE 	;TxQueue elements are byte sized
	CALL	QueueInit 		;call the function QueueInit to initialize 
                                	;TxQueue using the passed in values from 
                                	;SI (queue address) and BL (queue element size);
                                	;the length argument is ignored 
	;JMP     InitKickStart 

InitKickStart:
	MOV	KickStarter, NO_KICKSTART_NEEDED ;initialize the KickStarter to
                                                 ;NO_KICKSTART_NEEDED 
	;JMP    InitSerialDone
	
InitSerialDone: 
	RET				;done so return				

InitSerial		ENDP


; SetBaudRate
;
; Description:		This function receives a passed in index (AX) of a 
;                   	baud rate divisor in BaudRateTable to access a baud 
;                   	rate divisor. The function writes the value of the 
;                   	baud rate divisor to the divisor latch register in 
;                   	order to write to the baud generator. The baud rate
;                   	divisors are all calculated assuming a 9.216 MHz crystal. 
;
; Operation:		The function receives the current LCR value and sets 
;                   	the DLAB bit of the LCR value in order to access the 
;                   	divisor latch register. The DLAB bit is set using a 
;                   	mask. Then the function looks up the corresponding 
;                   	baud rate divisor from BaudRateTable and writes the 
;			baud rate divisor to the divisor latch register one 
;                   	byte at a time. Afterwards the original LCR value is 
;                   	restored to be outputted so the divisor latch register
;                   	access is disabled. 
;
; Arguments:		baud rate divisor (AX) - index of a baud rate divisor
;                                            	 referenced in BaudRateTable; 
;                                            	 value ranges from 0 to 13 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	None 
;
; Global Variables:	None 
;
; Input:		LCR value - 8 bit value to read from the line control
;                                   register  
;
; Output:		LCR value - 8 bit value to write to the line control 
;                                   register 
;                  	Divisor latch register - 8 bit value to write to the 
;                                            	 divisor latch register in 
;                                            	 order to write to the baud 
;                                            	 generator 
;
; Error Handling:	None 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed: None 
;
; Limitations:		Assumes a valid argument in AX 
;
; Author: Yuan Ma 
; Last Modified: 11/22/15
;


SetBaudRate      	PROC        NEAR
                	PUBLIC      SetBaudRate
	
	PUSHA                   ;save all of the registers 
	PUSHF                   ;save the flag register 
	CLI		        ;critical code so we disable interrupts 
 
SetBaudRateInit: 
	MOV	BX, AX 		;store baud rate divisor index in another
				;register so we can use AX to receive input 
	SHL	BX, TABLE_WORD_INDEXED  ;shift index since BaudRateTable 
                                        ;is indexed as words 
	;JMP    AccessDLAB      ;now we need to access the divisor latch register 

AccessDLAB: 
	MOV	DX, LCR_ADDRESS ;set up DX with the LCR address in order to 
				;receive input from the LCR address 
	IN	AL, DX		;get the current value that is written to the 
				;line control register 
	PUSH	AX		;save the current value written to LCR so 
				;we can output it later 
	OR	AL, DLAB_MASK	;mask the LCR value so we can set the DLAB bit 
	OUT	DX, AL          ;output this value to enable access to the 
				;divisor latch register 
    ;JMP    SetBaudRateDivisor  ;now we write the baud rate divisor to the 
                                ;divisor latch register 

SetBaudRateDivisor: 
	MOV	DX, LSB_ADDRESS	;set up DX with the divisor latch 
                                ;address in order to write values 
                                ;to the divisor latch 
	MOV	AX, CS:BaudRateTable[BX];using the passed in index value, 
                                        ;look up corresponding baud rate 
                                        ;divisor value from BaudRateTable 
	INC     DX              ;since we must write the value one
				;byte at a time, increment DX so we 
                                ;can write the high byte 
	MOV     AL, AH          ;move the high byte of the baud 
                                ;rate divisor into AL so it can 
                                ;be outputted 
	OUT     DX, AL          ;output baud rate divisor to 
                                ;the divisor latch address 
	;JMP    DisableDLAB     ;now that we are done, we must 
                                ;disable divisor latch register access 

DisableDLAB: 
	MOV	DX, LCR_ADDRESS ;set up DX with the LCR address in order to 
                                ;output the original LCR value 
	POP	AX	        ;restore the previously saved original LCR value 
	OUT	 DX, AL         ;output this value to disable access to the 
                                ;divisor latch register 
	;JMP    SetBaudRateDone 

SetBaudRateDone: 
	POPF                    ;restore flags (interrupt flag is restored
                                ;so interrupts are enabled again) 
	POPA                    ;restore registers 
	RET			;done, so return 

SetBaudRate		ENDP




; SetParity
;
; Description:		This function receives a passed in index (AX) of a 
;                   	type of parity in ParityTable that will be written to the 
;                   	LCR value in order to enable the parity referenced by 
;                   	the ParityTable.  		      
;
; Operation:		This function sets the parity bits of the LCR value as
;                   	determined by the type of parity indicated by the passed
;                   	in value AX. The function first receives the current LCR  
;                   	value and or-s this LCR value with the type of parity 
;                   	indicated by the passed in value AX. This will ensure that
;                   	the bits that don't have to do with parity are not changed. 
;                   	This new value is then outputted to the LCR so the parity
;                   	bits have now been set. 
;
; Arguments:		parity type (AX) - index of a parity type referenced in 
;                                          ParityTable; values range from 0 to 4 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	None  
;
; Global Variables:	None
;
; Input:		LCR value - 8 bit value to read from the line control
;                                   register  
;
; Output:		LCR value - 8 bit value to write to the line control 
;                                   register with parities set 
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    None 
;
; Limitations:		Assumes a valid argument in AX  
;
; Author: Yuan Ma
; Last Modified: 11/22/15
;


SetParity   	PROC        NEAR
                PUBLIC      SetParity

	PUSHA                           ;save registers 
	MOV	 BX, AX                 ;store the parity index in another register  
                                	;so we can use AX to receive input 
	MOV	DX, LCR_ADDRESS         ;set up DX with the LCR address in order 
                                	;to receive input from the LCR address 
	IN      AL, DX                  ;get the current value that is written to 
                                	;the line control register 
	AND     AL, PARITY_MASK         ;and with the parity mask to clear the 
                                	;parity bits first and not change other bits 
	OR	AL, CS:ParityTable[BX]  ;set the parity bits by or-ing with the 
                                	;type of parity indicated by ParityTable 
                                	;this will ensure non-parity bits are not
                                	;changed 
	OUT	DX, AL                  ;output the new LCR value to the LCR address
                                	;with the parity bits now set 
	POPA                            ;restore registers 
  	RET                             ;done so return 


SetParity	ENDP



; SerialPutChar	
;
; Description:		This function indirectly outputs the passed in character (c)
;			to the serial channel by enqueueing the character to the 
;                   	TxQueue (it is later transmitted in the SerialEventHandler). 
;                   	The carry flag will be reset if the character has been 
;                   	output and set otherwise. The function will also kickstart 
;                   	the transmitter holding register empty interrupt if needed.        
;
; Operation:		The function first checks if the TxQueue is full or not
;			by calling function QueueFull from HW3. If the queue is 
;			full (zero flag set), no more characters can be enqueued
;                   	so the carry flag will also be set to indicate a character
;                   	has not been output. If the queue is not full (zero 
;                   	flag unset), the passed in character will be enqueued by 
;                   	calling the function Enqueue from HW3 and the carry 
;                   	flag will be reset to indicate a character has been 
;                   	successfully enqueued to be outputted later. The function 
;                   	will also check if the we need to manually kickstart the 
;                   	transmitter holding register empty (THRE) interrupt. If a 
;                   	kickstart is needed, the function manually disables the 
;                   	THRE interrupt and then enables it again to make sure we
;                   	will receive the interrupt more than once. 
;
; Arguments:		c (AL) - character to be outputted to the serial 
;				 channel.  
;
; Return Value:		Carry flag is reset if c has been enqueued to TxQueue  
;			carry flag is set if c has not been outputted
;
; Local Variables:	None
;
; Shared Variables: 	TxQueue - queue of elements to be transmitted
;				  over the serial with byte sized
;				  elements (written to and read) 
;                   	KickStarter - indicates whether we need to manually 
;                                     kickstart the transmitter empty interrupt. 
;                                     (read from and written to) 
;
; Global Variables:	None
;
; Input:		IER value - 8 bit value to read from the interrupt enable
;                              	    register if kickstart is needed 
;
; Output:		IER value - 8 bit value to write to the interrupt enable
;                                   register with THRE interrupt disabled and then
;                                   re-enabled if kickstart is needed 
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    None 
;
; Limitations:	    None	
;
; Author: Yuan Ma
; Last Modified: 11/22/15
;


SerialPutChar  	PROC        NEAR
                PUBLIC      SerialPutChar

	PUSHA	                    ;save registers (including passed in value) 
    
CheckTxQueue:
	MOV	SI, OFFSET(TxQueue) ;set up SI as the address of the TxQueue 
                                    ;which is needed as the argument for QueueFull 
	CALL	QueueFull	    ;call QueueFull check if queue is full 
	JZ	DoNotEnqeueChar	    ;the zero flag was set indicating the 
                                    ;queue is full so we cannot enqueue a character
	;JNZ	EnqueueChar  	    ;the zero flag was not set indicating the
                                    ;queue is not full so we will enqueue the 
                                    ;character to TxQueue to be outputted 

EnqueueChar: 
	CALL	Enqueue             ;enqueue the passed in character from AL 
	CMP     KickStarter, KICKSTART_NEEDED  ;check if we need to manually 
                                               ;kickstart the transmitter 
                                               ;empty interrupt 
	JNE     NoKickStart         ;kickstart is not set, we don't need to kickstart  
	;JE     NeedKickStart       ;if kickstart is set, we need to kickstart   

NeedKickStart: 
	MOV     KickStarter, NO_KICKSTART_NEEDED    ;reset kickstarter since we
                                		    ;will deal with kickstarting now 
	MOV     DX, IER_ADDRESS     ;set up DX with the IER address in order 
                                    ;to receive input from the IER address 
	IN      AL, DX              ;get the current value that is written to 
                                    ;the interrupt enable register 
	AND     AL, DISABLE_THRE_INT ;and the current value with DISABLE_THRE_INT
				     ;in order to disable the THRE interrupt and 
                                    ;not change any of the other bits 
	OUT     DX, AL              ;output the new IER value to the IER address
                                    ;with the THRE interrupt disabled 
    
	IN      AL, DX              ;get the current value that is written to 
                                    ;the interrupt enable register 
	OR      AL, ENABLE_THRE_INT ;or the current value with ENABLE_THRE_INT
                                    ;in order to enable the THRE interrupts and 
                                    ;not change any of the other bits 
	OUT     DX, AL              ;output the new IER value to the IER address
                                    ;with the THRE interrupt enabled 
	;JMP    ClearCarryFlag      ;we still need to clear the carry flag
                                    ;to indicate character has been enqueued 
    
NoKickStart: 
	CLC                         ;clear the carry flag to indicate the 
                                    ;character has been enqueued to TxQueue 
                                    ;to eventually be outputted 
	JMP     SerialPutCharDone
    
DoNotEnqeueChar:
	STC			    ;set the carry flag to indicate character
                                    ;has not been output 
	;JMP    SerialPutCharDone

SerialPutCharDone:
 	POPA                        ;restore the registers 
	RET                         ;done, so return 


SerialPutChar	ENDP



; SerialPutString 	
;
; Description:		This function is passed a null terminated string in ES:SI
;			that it indirectly outputs to the serial channel by enqueueing
;			one character of the string at a time to the TxQueue (the 
;			characters are later transmitted in the SerialEventHandler). 
;			If a character can’t be send over, meaning TxQueue is full,
;			the function will just exit and return. After the entire 
;                  	string is sent over, the carriage return is sent to  
;			indicate the end of the string and will reset the parser. 
;				        
;
; Operation:		This function sets up BX as the index of the characters in 
;			the string to be sent over the serial channel. Starting from 
;			the first character in the string, the function will set up
;			AL (character argument for SerialPutChar) with the character
;			of the string and check if it is the null terminator. If so,
;			the carriage return is sent to indicate the end of the string. 
;			If not, the function SerialPutChar is called with AL set up
;			as the character to be enqueue to TxQueue. If the carry flag 
;			is set, indicating the character of the string cannot be 
;			enqueued to TxQueue, the function will just exit and return. 
;			Otherwise, if the carry flag is not set, indicating the 
;			character has been successfully enqueued to TxQueue, we will 
;			increase the index to look at the next character in the string
;			and loop back to check this next character.  
;
; Arguments:		ES:SI - string to be sent over the serial channel 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables: 	None  
;
; Global Variables:	None
;
; Input:		None  
;
; Output:		None 
;
; Error Handling:	If a character of the string cannot be enqueued to TxQueue, 
;			the carry flag will be set through SerialPutChar and the
;			function just returns. 
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: 	None 
;
; Limitations:	   	None	
;
; Author: Yuan Ma
; Last Modified: 12/4/15
;


SerialPutString  	PROC        NEAR
                	PUBLIC      SerialPutString

	PUSHA		        	;save registers

SerialPutStringInit:
	MOV	BX, 0 			;start index of the string to transmit at the 
                                	;first character  
	;JMP	SerialPutStringLoop 	;start looping from first character 

SerialPutStringLoop:
	MOV	AL, ES:[SI+BX] 		;look at character in string indexed by BX
	CMP	AL, ASCII_NULL     	;check if we have reached the end of the string
                                	;which is indicated by ASCII_NULL
	JE	SerialPutStringEOS	;if so, we send the carriage return ASCII char
                                	;to indicate reaching the end of the string 
	;JNE	SendOneChar		;if not, we can send this character over 

SendOneChar:
	CALL	SerialPutChar		;call function SerialPutChar to transmit 
                                	;character which is already in AL
	JC	SerialPutStringDone 	;if carry flag is set, meaning character not 
                                	;successfully sent over serial, just exit 
	;JNC 	SerialPutNextChar	;if carry flag not set, meaning character 
                                	;successfully sent over serial, we can now
                                	;send the next character over

SerialPutNextChar: 
	INC	BX			;increment index to next character in string 
	JMP	SerialPutStringLoop	;loop back up to check the next character

SerialPutStringEOS:
	MOV	AL, ASCII_EOS 	    	;send over the EOS which is carriage return to 
                                	;indicate end of string
	CALL	SerialPutChar		;send this over the serial channel 
	;JMP	SerialPutStringDone 	;done sending the entire string  

SerialPutStringDone:
	POPA				;restore registers 
	RET 



SerialPutString		ENDP



; SerialEventHandler	
;
; Description:		This function handles events for the serial channel 
;			using INT2 interrupts. The function will continuously 
;                   	loop and read in values from the IIR register. If an 
;                  	interrupt is detected, the function will look up the 
;                   	interrupt type from InterruptJumpTable and jump to the 
;                   	appropriate section of code to handle each interrupt. 
;                   	There are four types of serial interrupts that can be 
;                   	handled: Modem Status, Transmitter Holding Register 
;                   	Empty, Received Data Available, and Receiver Line Status
;                   	interrupts. If an interrupt is not detected, the 
;                   	function will just send an EOI to clear out the interrupt
;                   	controller.   
;
; Operation:		This function runs continuously while interrupts are 
;			being generated from the serial chip and reads in values
;                   	from the IIR register. Once the function recognizes that 
;                   	value is not NO_INTERRUPTS, it will look up the interrupt
;                   	type from InterruptJumpTable which tells the function 
;                   	which portion of the code to jump to depending on the 
;                   	interrupt type. The function will handle four different
;                   	kinds of serial interrupts as described below before 
;                   	each block of code is jumped to. an EOI will be sent to
;                   	clear out the interrupt controller at the end so the 
;                   	function can wait for the next interrupt.  
;
; Arguments:		None 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	TxQueue - queue of elements to be transmitted
;				  over the serial with byte sized
;				  elements (read from and written to) 
;                   	KickStarter - indicates whether we need to manually 
;                                     kickstart the transmitter empty interrupt. 
;                                     (written to) 
;
; Global Variables:	None
;
; Input:		IIR value - 8 bit value to read from the interrupt 
;                                   identification register to check if an 
;                                   there is an interrupt that needs to be handled 
;
; Output:		None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed:    None 
;
; Limitations:		Jump table only works with specific IIR bit pattern.  
;
; Author: Yuan Ma
; Last Modified: 11/22/15
;


SerialEventHandler  	PROC        NEAR
                        PUBLIC      SerialEventHandler
  
    PUSHA                            ;save all the registers
    PUSHF                            ;save the flag register 
    
CheckForInterrupts:
    MOV     DX, IIR_ADDRESS         ;set up DX with the IIR address in order to
                                    ;receive input from the IIR address 
    IN      AL, DX                  ;get the current value that is written to 
                                    ;to the interrupt identification register 
    CMP     AL, NO_INTERRUPTS       ;check if this value is the same as the 
                                    ;value for if there are no interrupts 
    JE      SerialEventHandlerDone  ;if there are no interrupts that need to
                                    ;be handled, we are done and can send an EOI
    ;JNE    CheckInterruptType      ;if there is an interrupt that needs to 
                                    ;be handled, we need to check what type 
                                    ;of interrupt it is 

CheckInterruptType:
    XOR     BX, BX                  ;clear the value of BX 
    MOV     BL, AL                  ;move the value written to the IIR 
                                    ;into BL so we can use table lookup 
    JMP     CS:InterruptJumpTable[BX]   ;look up the type of interrupt that 
                                        ;we need to handle 
          
          
; ModemStatusInterrupt
; 
; Description:      This part of the subroutine handles the modem status interrupt.  
;                   This is called when the last three bits of the 
;                   value written to the IIR are all cleared (corresponding to 
;                   a table index value of 0). 
; 
; Operation:        To reset this interrupt, the subroutine will just read in 
;                   the value of the modem status register 
; 
; Input:            MSR value - 8 bit value to read from the modem status
;                               register 
; 
; Output:           None 

ModemStatusInterrupt: 
    MOV     DX, MSR_ADDRESS         ;set up DX with the MSR address in order to
                                    ;receive input from the MSR address 
    IN      AL, DX                  ;read the current value that is written 
                                    ;to the modem status register 
    JMP     CheckForInterrupts      ;done handling the interrupt, check if
                                    ;there are more interrupts 
    

; TransmitterEmptyInterrupt
; 
; Description:      This part of the subroutine handles the transmitter holding 
;                   register empty interrupt. This is called when 
;                   bit 0 and bit 2 are cleared and bit 1 is set of the 
;                   value written to the IIR (corresponding to a table 
;                   index value of 2).
; 
; Operation:        To reset this interrupt, the subroutine will check if 
;                   TxQueue is empty (zero flag will be set) by calling 
;                   function QueueEmpty from  HW3 and indicate a kickstart 
;                   is needed if TxQueue is empty. If TxQueue is not empty 
;                   (zero flag is not set) and there are characters available,
;                   a character will be dequeued from TxQueue using the 
;                   function Dequeue from HW#3 and this character is 
;                   outputted to the serial channel through the transmitter 
;                   holding register 
; 
; Input:            None 
; 
; Output:           THR value - 8 bit value to write to the transmitter holding
;                               register 

TransmitterEmptyInterrupt:
    MOV     SI, OFFSET(TxQueue)     ;set up SI as the address of the TxQueue 
                                    ;which is needed as the argument for 
                                    ;QueueEmpty 
    CALL    QueueEmpty              ;call QueueEmpty to check if the queue is
                                    ;empty 
    JZ      SetKickstart            ;the zero flag is set indicating the 
                                    ;queue is empty and there is no data to
                                    ;transmit so we need to set the kickstart 
    ;JNZ    OutputToTHR             ;the zero flag is not set indicating the 
                                    ;queue is not empty so there is data 
                                    ;available to output 
OutputToTHR:
    MOV     SI, OFFSET(TxQueue)     ;set up SI as the address of the TxQueue 
                                    ;which is needed as the argument for Dequeue
    CALL    Dequeue                 ;call Dequeue to dequeue a character from
                                    ;TxQueue, this value is now stored in AL 
    MOV     DX, THR_ADDRESS         ;set up DX with the THR address in order to
                                    ;output a value to the THR address 
    OUT     DX, AL                  ;output the dequeued character from
                                    ;TxQueue to THR to be transmitted over 
                                    ;the serial channel 
    JMP     CheckForInterrupts      ;done handling the interrupt, check if 
                                    ;there are more interrupts 
                                   
SetKickstart:
    MOV     KickStarter, KICKSTART_NEEDED          ;we need to set the kickstart
                                                   ;since TxQueue is empty 
    JMP     CheckForInterrupts      ;done handling the interrupt, check if 
                                    ;there are more interrupts 
                                    
                                    
; ReceiveDataInterrupt
; 
; Description:      This part of the subroutine handles the received data available 
;                   interrupt. This is called when bit 
;                   bit 0 and bit 1 are cleared and bit 2 is set of the 
;                   value written to the IIR (corresponding to a table 
;                   index value of 4).
; 
; Operation:        To reset this interrupt, the subroutine will get the 
;                   current value of the receiver buffer register and call 
;                   the function EnqueueEvent to enqueue this value with 
;                   RX_EVENT to indicate the event type. 
; 
; Input:            RBR value - 8 bit value that is read from the receiver 
;                               buffer register 
; 
; Output:           None 
   
ReceiveDataInterrupt:
    MOV     DX, RBR_ADDRESS         ;set up DX with the RBR address in order 
                                    ;to receive input from the RBR address
    IN      AL, DX                  ;get the current value that is written to
                                    ;the receiver buffer register 
    MOV     AH, RX_EVENT            ;set up AH as the RX_EVENT code to 
                                    ;indicate the event type 
    CALL    EnqueueEvent            ;call EnqueueEvent to enqueue the 
                                    ;value from the RBR address along with
                                    ;the event code: EnqueueEvent(AL, RX_EVENT)
    JMP     CheckForInterrupts      ;done handling the interrupt, check if 
                                    ;there are more interrupts 
                                    
                                    
; ErrorInterrupt
; 
; Description:      This subroutine handles the receiver line status (or error)
;                   interrupt. This subroutine is called when bit 
;                   bit 0 is cleared and bit 1 and bit 2 are set of the 
;                   value written to the IIR (corresponding to a table 
;                   index value of 6).
; 
; Operation:        To reset this interrupt, the subroutine will get the 
;                   current value of the line status register and mask out 
;                   all of the non-error bits. Then the subroutine will call 
;                   the function EnqueueEvent to enqueue this
;                   value with ERROR_EVENT to indicate the event type. 
; 
; Input:            LSR value - 8 bit value that is read from the line status 
;                               register 
; 
; Output:           None  
   
ErrorInterrupt: 
    MOV     DX, LSR_ADDRESS         ;set up DX with the LSR address in order
                                    ;to receive input from the LSR address 
    IN      AL, DX                  ;get the current value that is written to 
                                    ;the line status register 
    AND     AL, ERROR_MASK          ;mask out bits that do not have to do 
                                    ;with errors 
    MOV     AH, ERROR_EVENT         ;set up AH as the ERROR_EVENT code to 
                                    ;indicate the event type 
    CALL    EnqueueEvent            ;call EnqueueEvent to enqueue the value
                                    ;from the LSR address along with the 
                                    ;event code: EnqueueEvent(AL, ERROR_EVENT) 
    JMP     CheckForInterrupts      ;done handling the interrupt, check if 
                                    ;there are more interrupts 

SerialEventHandlerDone:
    MOV	    DX, INTCtrlrEOI	    ;send an INT2 EOI to clear out the controller 
    MOV	    AX, INT2EOI		      
    OUT	    DX, AL
    POPF                            ;restore flags        
    POPA                            ;restore registers 
    IRET                            ;done so return (IRET is used for interrupt
                                    ;event handlers) 


SerialEventHandler	ENDP




CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'


TxQueue	QUEUESTRUC <> 	;queue with byte sized elements where elements are 
                        ;stored before being transmitted over the serial 
                        ;(read from and written to)
KickStarter   DB  ?     ;indicates whether we need to manually kickstart the 
                        ;transmitter empty interrupt so we receive the 
                        ;interrupt more than once (read from and written to)

DATA    ENDS

        END       
