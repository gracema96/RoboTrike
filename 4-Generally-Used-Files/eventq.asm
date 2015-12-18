        NAME    EVENTQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    EVENTQ                                  ;
;                             Event Queue Functions                          ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: 
;	This file contains the event queue functions necessary for the 
;	RoboTrike Remote and Motor Main Loop and their functional 
;	specifications: InitEventQueue, EventQueueEmpty, EventQueueFull, 
;	EnqueueEvent, DequeueEvent, InitCriticalError, and GetCriticalError. 
;	Together, these functions initialize the event queue and allow 
;	for events for to be queued and dequeued. 
;
; Table of Contents:
;	InitEventQueue:		initializes an event queue 
;	InitCriticalError:	initializes critical error flag 
;	EventQueueEmpty:	checks if the event queue is empty 
;	EventQueueFull:		checks if the event queue is full
;	EnqueueEvent:		enqueues an event to the event queue
;				as long as it is not full
;	DequeueEvent:		dequeues an event from the event queue
;				as long as it is not empty 
;	GetCriticalError:	returns current critical error flag value 
;
; Revision History:
;     11/30/15  Yuan Ma		wrote outline
;     12/1/15	Yuan Ma		wrote functions
;     12/4/15	Yuan Ma		updated DequeueEvent and added comments 
;     12/12/15  Yuan Ma         updated a few comments/functions  
;


; include files
$INCLUDE(queue.inc) 		    ;queue constants 
$INCLUDE(general.inc)		    ;includes critical error flag constants 


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

	EXTRN	QueueInit:NEAR      ;initializes an empty queue at a specified 
                                    ;address with byte or word sized queue elements 
	EXTRN	QueueEmpty:NEAR	    ;determines whether or not a queue at a 
                                    ;specified address is empty or not; will 
                                    ;set the zero flag to indicate empty 
	EXTRN	QueueFull:NEAR      ;determines whether or not a queue at a 
                                    ;specified address is full or not; will set
                                    ;the zero flag to indicate full 
	EXTRN	Dequeue:NEAR        ;removes and returns the element at the 
                                    ;beginning of a queue at a specified address 
	EXTRN	Enqueue:NEAR        ;adds a passed in value to the end of a queue
                                    ;at a specified address 

		

; InitEventQueue  
;
; Description:		Initializes an empty EventQueue with word
;			sized queue elements, ignored length, and at 
;			address specified by SI. 
;
; Operation:		Sets up BX with QUEUE_WORD_SIZE to initialize 
;			a queue with word sized elements and then sets 
;			up SI with the address to start EventQueue. 
;			Finally function QueueInit from HW3 is called
;			to initialize the empty array. Length is ignored. 
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	EventQueue - queue to store events (written to) 
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
; Registers Changed:	AX, BX, SI 
;
; Limitations:		None 	  	
;
; Author: Yuan Ma 
; Last Modified: 11/30/15
;

InitEventQueue     	PROC        NEAR
                	PUBLIC      InitEventQueue

	MOV     BX, QUEUE_WORD_SIZE	;sets up BX with word-sized queue elements 
	MOV     SI, OFFSET(EventQueue)  ;set SI to address to start EventQueue 
	CALL    QueueInit		;call function QueueInit to 
                                        ;initialize EventQueue using the passed 
                                        ;in values from SI (queue address) and 
                                        ;BX (queue element size) while ignoring 
                                        ;queue length 
	RET				

InitEventQueue		ENDP


; EventQueueEmpty
;
; Description:		This function checks if EventQueue is empty. If 
;			it is empty, the zero flag will be set. Otherwise, 
;			the zero flag is reset. 		
;
; Operation:		Sets up SI with the address of EventQueue and then
;			calls the function QueueEmpty from HW3 to actually
;			check if EventQueue is empty. 
;
; Arguments:		None 
;
; Return Value:		ZF - sets the zero flag if EventQueue is empty 
;			     and resets otherwise  
;
; Local Variables:	None
;
; Shared Variables:	EventQueue - queue to store events (read)  
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
; Registers Changed:	SI, flags 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/30/15
;

EventQueueEmpty      	PROC        NEAR
                        PUBLIC      EventQueueEmpty

	MOV     SI, OFFSET(EventQueue)  ;set up SI as the address of EventQueue 
	CALL	QueueEmpty 		;call function QueueEmpty
                                        ;to check if EventQueue is 
                                        ;empty using the passed in address argument
                                        ;will set the zero flag if 
                                        ;empty and reset otherwise 
	RET				

EventQueueEmpty		ENDP


; EventQueueFull 
;
; Description:		This function checks if EventQueue is full. If 
;			it is full, the zero flag will be set. Otherwise, 
;			the zero flag is reset. 		
;
; Operation:		Sets up SI with the address of EventQueue and then
;			calls the function QueueFull from HW3 to actually
;			check if EventQueue is full. 
;
; Arguments:		None 
;
; Return Value:		ZF - sets the zero flag if EventQueue is full 
;			     and resets otherwise  
;
; Local Variables:	None
;
; Shared Variables:	EventQueue - queue to store events (read)  
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
; Registers Changed:	SI, flags 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/30/15
;

EventQueueFull      	PROC        NEAR


	MOV     SI, OFFSET(EventQueue)  ;set up SI as the address of EventQueue 
	CALL	QueueFull 		;call function QueueFull to check if 
                                        ;EventQueue is full using the passed in 
                                        ;address argument will set the zero flag if 
                                        ;full and reset otherwise
	RET				

EventQueueFull		ENDP


; EnqueueEvent 
;
; Description:		This function enqueues an event to the EventQueue
;			as long as the queue isn’t full. If an event 
;			can’t be enqueued, the critical error flag will be set.	
;
; Operation:		Sets up SI with the address of EventQueue and then 
;			calls the function QueueFull to check if the 
;			EventQueue is full. If the zero flag is set (indicates
;			the EventQueue is full), the shared variable 
;			CritErrorFlag will be set. Otherwise, if the zero flag 
;			isn’t set, the function Enqueue will be called to 
;			enqueue the event to EventQueue. 
;
; Arguments:		AX - event we wish to enqueue to EventQueue 
;                            (AH contains the event type and AL contains the 
;                             event value) 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	EventQueue - queue to store events (written)
;			CritErrorFlag - nonzero to indicate that there was
;					a critical error, otherwise zero
;					(written to)  
;
; Global Variables:	None 
;
; Input:		None
;
; Output:		None
;
; Error Handling:	This functions calls EventQueueFull to check if 
;                       the EventQueue full. If the EventQueue is full (zero flag
;                       set, the CritErrorFlag is set. 
;
; Algorithms:		None
;
; Data Structures: 	None 
;
; Registers Changed:	AX, SI, flags 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 11/30/15
;
;

EnqueueEvent      	PROC        NEAR
                	PUBLIC      EnqueueEvent

CheckEventQueueFull:  
	CALL	EventQueueFull	    ;call function EventQueueFull to check
                                    ;if the EventQueue is full
	JZ	SetCritErrorFlag    ;if the zero flag is set, indicating
                                    ;the EventQueue is full, we need to 
                                    ;indicate that this is a critical error 
	;JNZ	CanEnqueueEvent	    ;otherwise we can successfully enqueue
                                    ;the event to EnqueueEvent 

CanEnqueueEvent:
	MOV     SI, OFFSET(EventQueue) 	;set up SI as the address of EventQueue 
	CALL	Enqueue			;call the function Enqueue with the 
                                    ;passed in address in SI and the value to 
                                    ;enqueue in AL to enqueue the event to EventQueue 
	JMP    EnqueueEventDone 	

SetCritErrorFlag:
	MOV    CritErrorFlag, SET_FLAG	;set the critical error flag
                                        ;to indicate a critical error 
	;JMP	EnqueueEventDone		

EnqueueEventDone:	
	RET				

EnqueueEvent		ENDP


; DequeueEvent 
;
; Description:		This function dequeues an event from EventQueue 
;			as long as it is not empty and returns the dequeued
;			value in AX. 				
;
; Operation:		Sets up SI with the address of EventQueue and then 
;			calls the function EventQueueEmpty to check if 
;			EventQueue actually contains elements. If the zero
;			flag is set (indicates EventQueue is empty), just 
;			return without dequeuing. Otherwise, if the zero
;			flag isn’t set, the function Dequeue from HW3 will 
;			be called to actually dequeue an event from EventQueue.
;
; Arguments:		None 
;
; Return Value:		AX - element from the head of EventQueue
;
; Local Variables:	None
;
; Shared Variables:	EventQueue - queue to store events (read)  
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
; Registers Changed:	SI, flags 
;
; Limitations:		None  	
;
; Author: Yuan Ma 
; Last Modified: 12/4/15
;
; 

DequeueEvent     	PROC        NEAR
                	PUBLIC      DequeueEvent

CheckEventQueueEmpty: 
	MOV     SI, OFFSET(EventQueue)	;set up SI as the address of EventQueue 
	CALL	EventQueueEmpty		;call function EventQueueEmpty
                                        ;to check if the EventQueue is empty
	JZ     DequeueEventDone		;if the zero flag is set, 
                                        ;indicating the EventQueue is 
                                        ;empty, we just return without
                                        ;dequeuing since there is nothing to dequeue 
	;JNZ	CanDequeueEvent		;otherwise, we can successfully
                                        ;dequeue an event 

CanDequeueEvent:
	CALL	Dequeue			;call function Dequeue with address 
                                        ;argument in SI to dequeue an event from
                                        ;head of EventQueue 
	;JMP	DequeueEventDone 		

DequeueEventDone:
	RET				

DequeueEvent		ENDP


; InitCriticalError
;
; Description:		This function initializes the critical error flag.
;
; Operation:		The shared variable CritErrorFlag is set to be RESET_FLAG. 	
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	CritErrorFlag - nonzero to indicate that there was
;					a critical error, otherwise zero
;					(written to)   
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
; Last Modified: 11/30/15
;

InitCriticalError      	PROC        NEAR
                        PUBLIC      InitCriticalError

	MOV	    CritErrorFlag, RESET_FLAG 	;initially no critical error 
	RET				

InitCriticalError	ENDP



; GetCriticalError 
;
; Description:		This function returns the value of the critical error flag.  		
;
; Operation:		The vale of the shared variable CritErrorFlag
;			is returned in AX.  
;
; Arguments:		None 
;
; Return Value:		AX - value of the critical error flag 
;
; Local Variables:	None
;
; Shared Variables:	CritErrorFlag - nonzero to indicate that there was a 
;					critical error, otherwise zero (read from)   
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
; Last Modified: 11/30/15
;

GetCriticalError      	PROC        NEAR
                        PUBLIC      GetCriticalError
                        
    	XOR     AX, AX              ;clears AX because we will be returning a 
                                    ;value in it 
	MOV    AL, CritErrorFlag    ;return the value of the critical error flag in AX 
	RET				

GetCriticalError	ENDP



CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'

EventQueue	QUEUESTRUC <> 	    ;queue with word sized elements 
                                    ;to store the events and their values 
CritErrorFlag 	DB      ?	    ;nonzero to indicate that there was a 
                                    ;critical error, otherwise zero

DATA    ENDS



        END       
