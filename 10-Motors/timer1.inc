;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  TIMER1.INC                                ;
;                         Include for Timer1 Functions                       ;
;                                   EE/CS 51				     ;
;				    Yuan Ma				     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
; 	This file contains the the definitions used in timer1 that are
;	necessary for Timer 1 initialization. 
;
; Revision History:
;     11/13/15	Yuan Ma     initial revision
;     11/15/15  Yuan Ma     added a few constants 
;
;
; Definitions for Timer1 functions: 
;
; Timer Definitions
;
; Timer1 register address:
Tmr1Ctrl	EQU	0FF5EH		;address of Timer1 Control Register
Tmr1MaxCntA	EQU	0FF5AH		;address of Timer1 Max Count A Register
Tmr1Count	EQU	0FF58H		;address of Timer1 Count Register

;Timer1 control register value:
Tmr1CtrlVal	EQU	0E001H		;value to write to Timer1 Control Register 
                                        ;1--------------- enable timer
                                        ;-1-------------- write to control
                                        ;--1------------- enable interrupts
                                        ;----000000------ reserved
                                        ;---0------0----- read only
                                        ;-----------0---- TMRIN0 is an enable
                                        ;------------00-- no prescaling
                                        ;--------------0- single counter mode
                                        ;---------------1 continuous mode

;Timer1 interrupt vector:
Tmr1Vec		EQU	18	        ;interrupt vector for Timer1 


;Interrupt Controller definitions 

;Interrupt Controller address: 
INTCtrlrCtrl	EQU	0FF32H		;address of interrupt controller for timer  

;Interrupt control register value:
INTCtrlrVal 	EQU	00001H		;set priority for timers to 1 and enable 
                                        ;000000000000---- reserved
                                        ;------------0--- enable timer interrupt
                                        ;-------------001 timer priority
TimerEOI	EQU	00008H		;same for all timers 

;Timing definition 
COUNT_PER_MS    EQU     2304            ;number of timer counts per 
                                        ;millisecond with asummed 18.432 MHz clock
TMR1_MAX_COUNT	EQU	540		;number of timer counts per millisecond 
                                        ;with a 18.432 MHz clock to produce an 
                                        ;interrupt once every (PWMCYCLETIME/SEVEN_BIT_RES)
                                        ;miliseconds; calculated by taking the 
                                        ;(PWMCYCLETIME * COUNT_PER_MS)/SEVEN_BIT_RES
PWMCYCLETIME    EQU     30              ;time in milliseconds that we designated a
                                        ;pwm cycle to be
SEVEN_BIT_RES   EQU     128             ;value for a 7-bit resolution 

