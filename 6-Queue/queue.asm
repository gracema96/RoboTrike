        NAME    QUEUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    QUEUE                                   ;
;                                Queue Functions                             ;
;                                   EE/CS 51			             ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains five queue functions for the RoboTrike and their
;      functional specifications: QueueInit, QueueEmpty, QueueFull,
;      Dequeue, and Enqueue. The functions create a working queue
;      structure that can enqueue and dequeue values of either byte or
;      word size.
;
;
; Table of Contents:
; 	QueueInit:	initializes an empty queue at a specified address  
;			of specified length with queue elements that
;			are either byte or word sized
;	QueueEmpty:	determines whether or not a queue at a specified
;			address is empty or not
;	QueueFull:	determines whether or not a queue at a specified
;			address is full or not
;	Dequeue:	removes and returns the element at the beginning
;		    	of a queue at a specified address
;	Enqueue:	adds a passed in value to the end of a queue at
;			a specified address 
;
;
; Revision History:
;     10/19/15  Yuan Ma     wrote outline
;     10/24/15	Yuan Ma	    wrote code for functions, revised functional
;     		     	    specifications for all functions 
;     10/25/15  Yuan Ma     updated comments 
;     12/12/15  Yuan Ma     some small commenting fixes 


$INCLUDE(queue.inc)


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; QueueInit 
;
; Description:		This function initializes and prepares an initially
; 			empty queue of passed length (l) at the passed
;			address (a). "l" represents the maximum number of
;			items that can be in the queue. The entries of the
;			queue can either be bytes (8-bits) or words (16-bits)
;			depending on the passed in element size (s). The l 
;                       argument is ignored. 
;
; Operation:		The function first determines the size of the queue
; 			entries. If the passed in size is non-zero, then the
;			the entries are words; otherwise the entries are bytes.
;			The function initializes an empty queue by setting the
;                       length of the array to the maximum size and setting both the
;			head and tail of the queue at the zero index of the array. 
;
; Arguments:		a (SI) - the address for the queue to be initialized at
; 			l (AX) - length that is the maximum number of items 
;			       	 that can be stored in the queue. Ignored.
;			s (BL) - size of the queue entries; if s is non-zero then
;			       	 entries are words, if s is zero then entries are
;				 bytes
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	None
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
; Data Structures:	Creates a queue struct through an array by setting index
;      			values for the queue head and queue tail pointers, setting
;			the queue length, and the element size of the queue. 
;
; Registers Changed:    None
;
; Limitations:		The length of the queue cannot be greater than the
; 			pre-fixed maximum queue length. The queue can only
;			hold elements that are bytes or words.
;
; Author: Yuan Ma 
; Last Modified: 10/24/15


QueueInit       PROC        NEAR
                PUBLIC      QueueInit

               
CheckEntrySize:
	CMP     BL, 0			;checks for the size of the queue entries
	JE      SetSizeByte		;if zero, then set the entry size to bytes
	;JNE	SetSizeWord		;otherwise, set entry size to words

SetSizeWord:
	MOV     [SI].elem_size, QUEUE_WORD_SIZE	;sets elem_size of the array to words 
        JMP     SetQueueLength  	        ;next, set the queue length 


SetSizeByte:
	MOV 	[SI].elem_size, QUEUE_BYTE_SIZE	;sets elem_size of the array to bytes
	;JMP	SetQueueLength		        ;next, set the queue length  

SetQueueLength:
        MOV     [SI].len, QUEUE_MAX_SIZE	;automatically sets the length
                                                ;of the queue to the default
                                                ;maximum size
        ;JMP    HeadTailInit                    ;initialize the head and tail 
    
HeadTailInit:
	MOV     [SI].head, 0	                ;head initially points to the 
	                                        ;first byte of array
	MOV     [SI].tail, 0	                ;tail also points to first 
	                                        ;byte of array in order to 
                                                ;initialize an empty array

EndQueueInit:
	RET				        ;empty array has been created 

QueueInit	ENDP


; QueueEmpty
;
; Description:		This function checks if a queue at the passed in
; 			address (a) is empty or not. If it is empty, the
;			zero flag is set. Otherwise, the zero flag is reset. 
;
; Operation:		This function checks if both the index value of the
; 			head and tail pointer are equal. If they are
;			both equal, they both point at the same index and
;			that means the queue is empty. Otherwise, the
;			queue is not empty. 
;
; Arguments:		a (SI) - the address of the queue to be checked
;
; Return Value:		ZF - sets the zero flag to 1 if the queue is
; 	 		     empty and 0 otherwise 
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
; Data Structures:	Queue  
;
; Registers Changed:    flags, CX
;
; Limitations:		None
;
; Author: Yuan Ma
; Last Modified: 10/24/15
;


QueueEmpty      PROC        NEAR
                PUBLIC      QueueEmpty

TestQueueEmpty:
        MOV     CX, [SI].head       ;moves head index value to CX for comparison
                                    ;since CMP cannot compare two addresses 
	CMP     CX, [SI].tail	    ;checks if the head and tail are
                                    ;are pointing to the same index
                                    ;value. If they are, the ZF will be
                                    ;set. If not, it will not be set.
	;JMP	EndQueueEmpty


EndQueueEmpty:			    ;have determined if the queue is
                                    ;empty or not 
	RET

QueueEmpty	ENDP


; QueueFull 
;
; Description:		This function checks if a queue at the passed in
; 			address (a) is full or not. If it is full the 
;			zero flag is set. Otherwise, the zero flag is reset.
;
; Operation:		This function checks if the queue tail index value
; 			is in front of the queue head index value. To do this,
;			the function checks if the tail index value plus
;			one queue element size modulus the length of the
;			queue is equal to the head index value of the queue.
;			If they are equal, then the tail points in front of the
;			head, signifying the queue is full. Otherwise, the
;		        queue is not full. 
;
; Arguments:		a (SI) - the address of the queue to be checked
;
; Return Value:		ZF - sets the zero flag to 1 if the queue is
; 	 		     full and 0 otherwise  
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
; Data Structures:	None 
;
; Registers Changed:    flags, AX, BX, DX  
;
; Limitations:		None 
;
; Author: Yuan Ma 
; Last Modified: 10/24/15
;


QueueFull       PROC        NEAR
                PUBLIC      QueueFull


TestQueueFull:
	MOV     BX, AX			;stores AX in BX so AX can be used for division 
	MOV     AX, [SI].tail		;moves the tail index value to AX
	ADD 	AX, [SI].elem_size	;adds the elem_size to get the full
                                        ;value to be divided (tail + elem_size)
	MOV     DX, 0			;clears remainder for division
	DIV 	[SI].len		;divides (tail + elem_size) by the length
	CMP     DX, [SI].head		;compares the remainder with the head
                                        ;index. If they are equal, that means the
                                        ;was in front of the head and the zero flag
                                        ;is set. If not, the zero flag is not set. This 
                                        ;also takes into account wrapping of the queue.
	MOV     AX, BX			;restores the value of AX 
	;JMP	EndQueueFull

EndQueueFull:				;have determined if the queue is full or not
	RET

QueueFull	ENDP



; Dequeue 
;
; Description:		This function removes an 8-bit or 16-bit value
; 			depending on the queue's entry size from the head
;			of the queue at the passed in address (a) and  
;			returns the value in AL if bytes and AX if words. 
;			This function is a blocking function, if the queue 
;			is empty, it waits until a value can be removed and
;			returned. 
;
; Operation:		This function calls and loops the QueueEmpty function 
; 			at the beginning until the zero flag is 0, thus 
;			signifying the queue is not empty. It then removes 
;			the value at the head of the queue and moves the head
;			pointer to the new head value of the queue while taking
;			into account wrapping of the queue by setting the
;			new head pointer to (current head index value + queue
;			element size) modulus length of the queue. It checks
;			the queue size to determine what to return. If the
;			queue size is bytes, the value is returned in AL,
;			otherwise if the queue size is words, the value is
;			returned in AX. 
;
; Arguments:		a (SI) - the address of the queue for the head
; 			       	 value to be removed
;
; Return Value:		AL - the value from the head of the queue if the
; 	 		     element size is bytes. Otherwise returns AX.
;			AX - the value from the head of the queue if the
;			     element size is words. Otherwise returns AL. 
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
; Data Structures:	modifies the queue struct by removing the head value
;      			and setting a new head index value 
;
; Registers Changed:    flags, AX, BX, CX, DX 
;
; Limittions:		None
;
; Author: Yuan Ma
; Last Modified: 10/24/15
;


Dequeue         PROC        NEAR
                PUBLIC      Dequeue

CheckIfEmpty:			
	CALL	QueueEmpty		;calls the function QueueEmpty
                                        ;to check if the queue is empty
	JZ      CheckIfEmpty    	;if it is empty, continue
                                        ;checking until it is not empty
	;JMP	CheckDequeueElementSize	;once the queue contains an
                                        ;element, must check the size of
                                        ;element to know what to dequeue

CheckDequeueElementSize:
        MOV     BX, [SI].head		;first, determines the index value of
                                        ;the element that needs to be removed
	TEST	[SI].elem_size, QUEUE_BYTE_SIZE	;check if the element size
                                                ;is a byte or not
	JNZ     DequeueByte		;if it is a byte, remove a byte
	;JZ 	DequeueWord		;otherwise, remove a word

    
DequeueWord:
	MOV     CL, [SI].queue[BX]	;removes the first byte of the word
                                        ;at the indexed value of the queue
                                        ;and temporarily stores in CL
	MOV     CH, [SI].queue[BX+1]	;removes the second byte of the word
                                        ;at the indexed value of the queue
                                        ;and temporarily stores in CH 
	;JMP	MoveQueueHead		;need to set the new value of the queue
                                        ;head
                                
DequeueByte:		
	MOV     CL, [SI].queue[BX]	;removes the byte that was at 
                                        ;the indexed value of the queue
                                        ;and temporarily stores in CL
	JMP     MoveQueueHead		;need to set the new value of the queue
                                        ;head 

MoveQueueHead:
	MOV     AX, [SI].head		;moves the head index value to AX for division 
	ADD 	AX, [SI].elem_size	;adds the elem_size to get the full
                                        ;value to be divided (head + elem_size)
	MOV     DX, 0			;clears remainder for division
	DIV 	[SI].len    	        ;divides (head + elem_size) by the length
	MOV     [SI].head, DX		;sets the new head value to the modulus
                                        ;of the division to take into account
                                        ;wrapping of the queue
	;JMP	CheckReturnRegister	;check the size of stehe element to determine
                                        ;which register to return to 

CheckReturnRegister:
	TEST	[SI].elem_size, QUEUE_BYTE_SIZE	;check if the element size
                                                ;is a byte or not
	JNZ     ReturnByte		;if it is a byte, return word in AL
	;JZ 	ReturnWord	        ;otherwise, return byte in AX

ReturnWord:
	MOV     AX, CX			;returns the temporarily stored
                                        ;dequeued word value to AX
	JMP	EndDequeue

ReturnByte:
	MOV     AL, CL			;returns the temporarily stored
                                        ;dequeued byte value to AL
	;JMP    EndDequeue

EndDequeue:				;done dequeueing and returning the
                                        ;head value 
	RET

Dequeue 	ENDP


; Enqueue 
;
; Description:		This function adds an 8-bit or 16-bit value (v)
; 			depending on the queue's entry size to the tail
;			of the queue at the passed in address (a).  This 
;			function is a blocking function, if the queue is
;			full, it waits until there is an empty space in
;		        the queue to add the value and doesn't return
;			until the value is added to the queue. 
;
; Operation:		This function calls and loops the QueueFull function 
; 			at the beginning until the zero flag is 0, thus 
;		        signifying the queue is not full. It then adds the 
;		        passed in value to the tail of the queue and moves 
;		        the tail pointer to the new tail value of the queue 
;                       while taking into account wrapping of the queue by 
;		        setting the new tail pointer to (current tail index value
;		        + queue element size) modulus length of the queue.
;
; Arguments:		a (SI) - the address of the queue to add a value
; 			       	 to the the tail of
;			v (AX) - the value to add to the queue if the
;			       	 element size of the queue is words
;			v (AL) - the value to add to the queue if the
;			       	 element size of the queue is bytes
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
; Data Structures:	modifiies the queue struct by enqueueing a value at
;			the queue tail and setting a new tail index value
;
; Registers Changed:    flags, AX, BX, DX

;
; Limittions:		None 
;
; Author: Yuan Ma
; Last Modified: 10/24/15	
;


Enqueue         PROC        NEAR
                PUBLIC      Enqueue
		
CheckIfFull:			
	CALL	QueueFull		;calls the function QueueFull
                                        ;to check if the queue is full
	JZ      CheckIfFull		;if it is full, continue
                                        ;checking until it is not full
	;JMP	CheckEnqueueElementSize	;once the queue is not full,
                                        ;must check the size of queue
                                        ;elements to know what to enqueue

CheckEnqueueElementSize:
        MOV     BX, [SI].tail		;first, determines the index value of
                                        ;where to enqueue
	TEST	[SI].elem_size, QUEUE_BYTE_SIZE	;check if the element size
                                                ;is a byte or not
	JNZ     EnqueueByte		;if it is a byte, enqueue a byte
	;JZ     EnqueueWord 	        ;otherwise, enqueue a word
    
EnqueueWord:	
	MOV     [SI].queue[BX], AL	;moves the first byte of the word to the
                                        ;tail of the queue
	MOV     [SI].queue[BX+1], AH	;moves the second byte of the word to
                                        ;tail of the queue
	JMP     MoveQueueTail		;need to set the new value of the queue tail
    
EnqueueByte:
	MOV     [SI].queue[BX], AL	;move the byte to the tail of the queue
	;JMP	MoveQueueTail		;need to set the new value of the queue tail

MoveQueueTail:
	MOV     AX, [SI].tail		;moves the tail index value to AX for division 
	ADD 	AX, [SI].elem_size	;adds the elem_size to get the full
                                        ;value to be divided (tail + elem_size)
	MOV	DX, 0		        ;clears remainder for division
	DIV 	[SI].len		;divides (tail + elem_size) by the length
	MOV     [SI].tail, DX		;sets the new tail value to the modulus
                                        ;of the division to take into account
                                        ;wrapping of the queue
	;JMP	EndEnqueue		

EndEnqueue:				;done enqueueing the value to the tail
                                        ;of the queue
	RET

Enqueue 	ENDP



CODE    ENDS


        END
