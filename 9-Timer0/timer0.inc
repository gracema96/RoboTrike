;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  TIMER0.INC                                ;
;                         Include for Timer0 Functions                       ;
;                                   EE/CS 51				     ;
;				     Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
; 	This file contains the the definitions used in timer.asm that are
;	necessary for Timer 0 initialization. 
;
; References: EHDEMO.inc by Glen George
;
; Revision History:
;     10/31/15	Yuan Ma		initial revision
;
;
; Definitions for Timer0 functions: 
;
; Timer Definitions
;
; Timer0 register address:
Tmr0Ctrl	EQU	0FF56H		;address of Timer0 Control Register
Tmr0MaxCntA	EQU	0FF52H		;address of Timer0 Max Count A Register
Tmr0Count	EQU	0FF50H		;address of Timer0 Count Register

;Timer0 control register value:
Tmr0CtrlVal	EQU	0E001H		;value to write to Timer0 Control 
                                        ;Register
                                        ;1--------------- enable timer
                                        ;-1-------------- write to control
                                        ;--1------------- enable interrupts
                                        ;----000000------ reserved
                                        ;---0------0----- read only
                                        ;-----------0---- TMRIN0 is an enable
                                        ;------------00-- no prescaling
                                        ;--------------0- single counter mode
                                        ;---------------1 continuous mode

;Timer0 interrupt vector:
Tmr0Vec		EQU	8		;interrupt vector for Timer0 


;Interrupt Controller definitions 

;Interrupt Controller address: 
INTCtrlrCtrl	EQU	0FF32H		;address of interrupt controller for timer 

;Interrupt control register value:
INTCtrlrCVal 	EQU	00001H		;set priority for timers to 1 and enable
                                        ;000000000000---- reserved
                                        ;------------0--- enable timer interrupt
                                        ;-------------001 timer priority
TimerEOI	EQU	00008H		;same for all timers 

;Timing definition 
COUNT_PER_MS	EQU	2304 		;number of timer counts per 
                                        ;millisecond with asummed 18.432 MHz clock
                                
