        NAME    KEYPAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    KEYPAD                                  ;
;                                Keypad Functions                            ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: 
; 	This file contains the keypad functions for the RoboTrike and their  
;	functional specifications: KeypadScanInit and KeypadScan. The 
;	functions initialize the keypad, scan the keypad for a press and  
;	debounce the currently pressed key. 
;	
;
; Table of Contents:
;	KeypadScanInit: This function initializes the variables for the 
;			KeyPadScan function
;	KeypadScan: 	This function is called by the timer event handler 
;			and checks for a new key being pressed if none are 
;			currently pressed or debounces the currently pressed key.	
;
;
; Revision History:
;     11/2/15  	Yuan Ma     wrote outline
;     11/4/15	Yuan Ma     wrote code and comments for functions and  
;			    updated functional specifications 
;     11/5/15   Yuan Ma     debugged and updated code 
;     11/6/15   Yuan Ma     updated comments  
;     12/12/15  Yuan Ma     changed some constants 


; local include files
$INCLUDE(keypad.inc)
$INCLUDE(events.inc) 

;external functional declaration
    EXTRN   EnqueueEvent:NEAR


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP
		

; KeypadScanInit 
;
; Description:		This function initializes the shared variables that are
;			necessary for the KeypadScan function by resetting the row
;			to be scanned to the first row, clearing the key that has 
;			been pressed and resetting the debounce time.	 
;                 
; Operation:		The function sets the current row to scan (CurrentRow)
;			to be the first row, the current key pressed (KeyPressed)
;			to be in the unpressed state, and resets the debounce 
;			counter (DebounceCntr) to the original debounce time.  
;
; Arguments:		None 
;
; Return Value:		None 
;
; Local Variables:	None
;
; Shared Variables:	CurrentRow - the current row to scan, will be set to the
;				     first row (written to) 
;			KeyPressed - the current key that has been pressed,
;				     will be set to be in the unpressed state
;				     (written to) 
;			DebounceCntr - the debounce counter, will be reset to
;				       the original debounce time (written to)
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
; Last Modified: 11/4/15
;


KeypadScanInit      PROC        NEAR
                    PUBLIC      KeypadScanInit


	MOV     CurrentRow, FIRST_ROW		;reset the current row to the first row 
                                                ;so when scanning, it will start 
                                                ;scanning from the first row 
	MOV     KeyPressed, UNPRESSED_STATE	;resets the current key that has been
                                                ;pressed to be in unpressed key so we
                                                ;can detect any pressed keys 
	MOV     DebounceCntr, DEBOUNCE_TIME 	;resets the debounce counter to the 
                                                ;original debounce time so we can 
                                                ;register future key presses 

	RET				

KeypadScanInit		ENDP



; KeypadScan
;
; Description:		This function will be called by the Timer0 event 
;			handler from HW4 approximately every millisecond.
;			The function will scan through the keypad one row
;                   at a time to detect if a key has been pressed. If 
;                   a key has been pressed, it will debounce the key and
;                   set the debounce count to DEBOUNCE_TIME. If a key
;                   held down, it will take into account key 
;			        debouncing with auto-repeat and set the debounce 
;                   counter to AUTOREPEAT_RATE. Two keys pressed in 
;                   different rows will only recognize the key that is 
;                   pressed first. Two keys pressed in the same row 
;                   will generate a new key. The value of this new 
;                   key will be determined by which keys are pressed 
;                   down (which corresponds to setting the bit of the 
;                   key).  The keys are each defined as such:  
;                   (bit 1) (bit 2) (bit 3) (bit 4) 
;                   0E        0d      0b      07
;                   1E        1d      1b      17
;                   2E        2d      2b      27
;                   3E        3d      3b      37
;                   The first digit represents the row number. The second
;                   digit represents the amount that has been subtracted
;                   from 15 when the bits have been set. For example, 
;                   pressing down only the 0E key would mean setting bit 
;                   1 (decimal value 1) and subtracting 1 from 15 which 
;                   results in 14 (E in hexadecimal). Pressing down 
;                   multiple keys would just set multiple bits for a 
;                   new key value.  
;
; Operation:		The function will scan through all NUM_ROWS of the
;			        keypad one row at a time, trying to detect if a key 
;                   has been pressed. We will need a mask to detect if a 
;                   key has been pressed since keys are only ROW_SHIFT_AMOUNT bits long, 
;                   so we are only interested in the lower nibble. If the 
;                   function does not detect a key press, the function will 
;                   reset the debounce counter and move on to check the next 
;                   row while taking into account wrapping around of the 
;                   rows (CurrentRow = CurrentRow mod NUM_ROWS). If the 
;                   function detects that a key has been pressed, we will 
;                   need to check if it is the same key we detected last time. 
;                   If we detected a different key than last time, we just reset  
;                   the debounce counter and set the key we just detected as the
;                   currently detected key. If we detect the same key as
;                   we detected last time, then we will decrement the debounce
;                   counter. If the debounce counter reaches zero, the key 
;                   value will be enqueued and the debounce counter will  
;			        be set to the repeat rate in order to take into account 
;			        key debouncing with auto-repeat. 
;
; Arguments:		None 
;
; Return Value:		None
;
; Local Variables:	None
;
; Shared Variables:	CurrentRow   - 	the current row that is being scanned 
;					                (read and written to)
;			        KeyPressed   - 	the current key that is being pressed 
;					                (read and written to)
;			        DebounceCntr - 	the time a key should be debounced for;
;                                   will be set to the auto-repeat rate if 
;                                   the function detects a key is held down 
;					                (read and written to)  
;
; Global Variables:	None
;
; Input:		    A keypad in a 4x4 array with 16 keys
;                   (A byte is read in from keypad I/O) 
;
; Output:		    None
;
; Error Handling:	None
;
; Algorithms:		None
;
; Data Structures:	None 
;
; Registers Changed: AX, BX, CX, DX, flags 
;
; Limitations:		The function ignores multiple key presses from different
;                   rows and assumes a 4x4 keypad. 
;
; Author: Yuan Ma
; Last Modified: 11/6/15
;
;



KeypadScan      PROC        NEAR
                PUBLIC      KeypadScan

                                ;scan through a row to see if we 
                                ;detect a key press 
ScanARow: 
	MOV	    DX, KEYPAD_ADDRESS	;we need the base keypad address to 
	ADD	    DL, CurrentRow		;which we will add the current row 
                                ;in order to get the keypad address 
                                ;of the row we are interested in 
	MOV	    DH, 0 			    ;we need to make sure the high bits 
                                ;are set to 0 since all keypad row 
                                ;addresses are only 80H - 83H 
	IN	    AL, DX			    ;from the keypad input, we will get 
                                ;the key values of the current row
                                ;AL must be used as specified by the 
                                ;IN instruction  
	AND	    AL, KEY_MASK		;since keys are only 4 bits long, we 
                                ;are only interested in the lower 
                                ;nibble so we need to mask out the 
                                ;values of the high nibble 
	CMP	    AL, UNPRESSED_STATE	;we will check if we detect a key to be pressed	
	JNE	    KeyIsPressed		    ;if we detect a key to be pressed, 
                                ;we need to check if it is the same
                                ;key that we detected last time
	;JE	NoKeyPressed		    ;otherwise we will check the next row


                                            ;if no key is pressed, we 
                                            ;will reset the debounce 
                                            ;counter and move on to check 
                                            ;the next row 
NoKeyPressed:
	MOV	    DebounceCntr, DEBOUNCE_TIME	    ;reset the debounce counter to 
                                            ;the original debounce time 
	INC	    CurrentRow 			            ;increment the current row 
                                            ;while taking into account 
                                            ;wrapping around of the rows 
	MOV	    AL, CurrentRow 			        ;we move the CurrentRow to AL 
                                            ;since the DIV instruction 
                                            ;only works on AX (we have 8 bits) 
	MOV	    AH, 0				            ;we clear AH to make sure we only
                                            ;divide the value in AL 
	MOV	    BL, NUM_ROWS			        ;we move the amount we want to divide
                                            ;by to BL, in this case it is the
                                            ;total number of rows in the keypad 
	DIV	    BL 				                ;we divide by BL
	MOV	    CurrentRow, AH 			        ;we set the CurrentRow to be the 
                                            ;remainder of this division
                                            ;CurrentRow = CurrentRow mod 
                                            ;NUM_ROWS
	JMP	    KeypadScanDone 			        ;we are done 
	 

                                
KeyIsPressed:
	CMP	    AL, KeyPressed		;we need to check if the key pressed 
                                ;is the same as the key we detected last time  
	JE	    DecrementDebounceCntr	;if it is the same key, then we will 
                                    ;decrement the debounce counter 	
	;JNE	ResetKeyandCount	;it it is not the same key, then we 
                                ;will reset the debounce counter and 
                                ;the current key pressed to the one 
                                ;we just detected

ResetKeyandCount:
	MOV	    DebounceCntr, DEBOUNCE_TIME	;reset the debounce time to the
                                        ;original debounce time
	MOV	    KeyPressed, AL			    ;set the current key pressed
                                        ;to the one we just detected 
	JMP	    KeypadScanDone			
 

DecrementDebounceCntr:
	DEC	    DebounceCntr		;we will decrease the debounce counter
                                ;since the same key has been pressed 
	CMP	    DebounceCntr, 0 	;we check if the key has been pressed 
                                ;long enough to register it as a key press
	JNE	    KeypadScanDone		;if not, then we are done 
	;JE	    EnqueueKey		    ;if so, then we need to enqueue this key 
                                ;and set the debounce counter to the
                                ;auto-repeat rate 

EnqueueKey: 
    MOV     CL, CurrentRow      ;we temporarily store the CurrentRow value 
                                ;in CL to operate on it 
    SHL     CL, ROW_SHIFT_AMOUNT        ;we shift the value of the row left 
                                        ;ROW_SHIFT_AMOUNT
    ADD     AL, CL              ;so that when we add it to the key value 
                                ;it won't mask the key value itself 
	MOV     AH, KEY_EVENT       ;EnqueueEvent requires a key event as an input 
	CALL	EnqueueEvent        ;we enqueue the key value in AL and key 
                                ;event in AH 
	MOV	    DebounceCntr, AUTOREPEAT_RATE   ;we set the debounce counter to the 
                                            ;auto-repeat rate 
	;JMP	KeypadScanDone 

KeypadScanDone: 
	RET 




KeypadScan	ENDP



CODE    ENDS



; Data segment

DATA    SEGMENT PUBLIC 'DATA'

CurrentRow 	    DB	?	;row that is scanned 
KeyPressed	    DB  ?	;current key that is pressed on the CurrentRow  
DebounceCntr	DW	?	;amount of time a key should be debounced for 

DATA    ENDS



        END       
