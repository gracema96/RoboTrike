;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   QUEUE.INC                                ;
;                           Include for Queue Function                       ;
;                                   EE/CS 51                                 ;
;                                   Yuan Ma 	                             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File Description:
;      This file contains the constants used in queue.asm and the definitions
;      for the queue structure itself. 
;
;
; Revision History:
;      10/24/15	     Yuan Ma	Initial revision
;      10/25/15      Yuan Ma    Added QUEUE_SIZE
;      12/12/15      Yuan Ma    small comment changes 
;
;
; Constant definitions for queue functions:

QUEUE_MAX_SIZE	EQU	256	;fixed maximum queue length that AX
                                ;cannot exceed, used as the default
                                ;length 
QUEUE_WORD_SIZE	EQU	2	;number of index values that a word
                                ;takes up in the array
QUEUE_BYTE_SIZE	EQU	1	;number of index values that a byte
                                ;takes up in the array
QUEUE_SIZE      EQU     QUEUE_MAX_SIZE - 1  ;size of array while leaving one 
                                            ;empty space 

; Definitions for queue structue:

QUEUESTRUC	    STRUC

        head	     DW   ?	;index for the head pointer of the queue,
                                ;points to the first element
	tail         DW   ?	;index for the tail pointer of the queue,
                                ;points to the last element
	len          DW   ?	;length of the queue, maximum number of
                                ;bytes in the queue (unused) 
	elem_size    DB   ?	;size of an element in the queue, either
                                ;bytes or words 
	queue 	     DB   QUEUE_MAX_SIZE DUP (?)  ;the actual array of the
                                                  ;queue that holds queue 
                                                  ;elements 

QUEUESTRUC         ENDS 
