        NAME    INTERRUPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    INTERRUPT                               ;
;                               Interrupt Functions                          ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:      
;       This program contains the functions to initialize the interrupt 
;       vector table and handles illegal events 
;              
;
; Table Of Contents:
;   ClrIRQVectors:          installs the illegal interrupt handler for 
;                           interrupt vectors in the interrupt vector table 
;
;   IllegalEventHandler:    Handles the illegal interrupts by doing nothing 
;
; References: EH.DEMO.asm by Glen George
; 
; Revision History:
;     10/30/15  Yuan Ma        initial revision 
;     11/1/15   Yuan Ma        updated comments 


CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP

        
; include files
$INCLUDE (int.inc) 


; ClrIRQVectors
;
; Description:      This functions installs the IllegalEventHandler for all
;                   interrupt vectors in the interrupt vector table.  Note
;                   that all 256 vectors are initialized so the code must be
;                   located above 400H.  The initialization skips  (does not
;                   initialize vectors) from vectors FIRST_RESERVED_VEC to
;                   LAST_RESERVED_VEC.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  CX    - vector counter.
;                   ES:SI - pointer to vector table.
;
; Shared Variables: None.
;
; Global Variables: None.
;
; Input:            None.
;
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
;
; Data Structures:  None.
;
; Registers Used:   flags, AX, CX, SI, ES
;
; Author:           Yuan Ma 
; Last Modified:    11/1/15


ClrIRQVectors   PROC    NEAR
                PUBLIC  ClrIRQVectors
                

InitClrVectorLoop:              ;setup to store the same handler 256 times

        XOR     AX, AX          ;clears AX
        MOV     ES, AX          ;initializes ES to 0 because we are also 
                                ;using ES to write to the interrupt vector table
        MOV     SI, 0           ;initialize SI to skip RESERVED_VECS 
                                ;(4 bytes each) 

        MOV     CX, NUM_IRQ_VECTORS         ;up to 256 vectors to initialize


ClrVectorLoop:                  ;loop clearing each vector
                                ;check if should store the vector

        CMP     SI, DOUBLE_WORD_SIZE * FIRST_RESERVED_VEC
        JB      DoStore         ;if before start of reserved field - store it
        CMP     SI, DOUBLE_WORD_SIZE * LAST_RESERVED_VEC
        JBE     DoneStore       ;if in the reserved vectors - don't store it
        ;JA     DoStore         ;otherwise past them - so do the store

DoStore:                        ;store the vector
        MOV     ES: WORD PTR [SI], OFFSET(IllegalEventHandler)
        MOV     ES: WORD PTR [SI + WORD_SIZE], SEG(IllegalEventHandler)

DoneStore:                      ;done storing the vector
        ADD     SI, DOUBLE_WORD_SIZE   ;since each entry in the interrupt vector 
                                ;table is 4 bytes, adding 4 to SI will move SI
                                ;to point to the next vector in the table 

        LOOP    ClrVectorLoop   ;loop until have cleared all vectors
        ;JMP    EndClrIRQVectors;and all done


EndClrIRQVectors:               ;all done, return
        RET


ClrIRQVectors   ENDP


; IllegalEventHandler
;
; Description:       This procedure is the event handler for illegal
;                    (uninitialized) interrupts.  It does nothing - it just
;                    returns after sending a non-specific EOI.
;
; Operation:         Send a non-specific EOI and return.
;
; Arguments:         None.
;
; Return Value:      None.
;
; Local Variables:   None.
;
; Shared Variables:  None.
;
; Global Variables:  None.
;
; Input:             None.
;
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
;
; Data Structures:   None.
;
; Registers Changed: None
;
; Author:            Yuan Ma 
; Last Modified:     11/1/15


IllegalEventHandler     PROC    NEAR
                        PUBLIC  IllegalEventHandler
        NOP                             ;do nothing (can set breakpoint here)

        PUSH    AX                      ;save the registers; since this is 
                                        ;an event handler, every register 
                                        ;changed by the function must be saved 
        PUSH    DX

        MOV     DX, INTCtrlrEOI         ;send a non-sepecific EOI to the
        MOV     AX, NonSpecEOI          ;interrupt controller to clear out
        OUT     DX, AL                  ;the interrupt that got us here
                                        ;because the interrupt controller will 
                                        ;only allow one interrupt from a source 

        POP     DX                      ;restore the registers
        POP     AX

        IRET                            ;and return


IllegalEventHandler     ENDP


CODE    ENDS

        END
