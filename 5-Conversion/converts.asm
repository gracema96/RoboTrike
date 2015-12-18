        NAME    CONVERTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   CONVERTS                                 ;
;                             Conversion Functions                           ;
;                                   EE/CS 51                                 ;
;                                    Yuan Ma                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description:
;      This file contains two functions with their functional specifications:
;      Dec2String and Hex2String. The functions can convert a signed 16-bit 
;      binary number to decimal or an unsigned 16-bit binary number to 
;      hexademical.
;
;
; Table of Contents:
;     Dec2String:   This function converts a passed in 16-bit signed value
;                   to a decimal value in a string represeantation with ASCII
;                   characters 
;     Hex2String:   This function converts a passed in 16-bit unsigned value to 
;                   a hexadecimal value in a string representation with ASCII
;                   characters 
;
; Revision History:
;     10/12/15  Yuan Ma      Wrote outline 
;     10/16/15	Yuan Ma	     Wrote Dec2String code and revised outline
;     10/17/15	Yuan Ma	     Wrote Hex2String code 
;     10/18/15  Yuan Ma      Final revisions to code
;     11/1/15   Yuan Ma      updated comments 
;     12/6/15   Yuan Ma      added general.inc 

; include files
$INCLUDE (converts.inc)
$INCLUDE (general.inc) 

CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP




; Dec2String
;
; Description: 		This function is passed a 16-bit signed value (n)  
;			and converts it to a decimal value in a string 
;			representation with ASCII characters. The string 
;			has 7 characters consisting of 5 numbers
;			preceded by a positive (+) or a negative (-) sign,
;			and is terminated with a <null>. The resulting string
;			is stored starting at the memory location indicated 
;			by the passed in address (a). 
;
; Operation:		The function starts by checking if the argument is 
;			positive or negative and assigns the appropriate 
;			sign. If negative, it will convert the argument to 
;			its absolute value. The function starts with the 
;			largest power of 10 possible and loops 
;			dividing the number by the decreasing powers of 10 
;			to get each digit. The digits are then converted to 
;			their ASCII representations by adding the value for 
;                       ASCII 0 to each digit and stored at the passed in 
;                       memory location (a). Every time a digit is stored, 
;                       the remainder of the division is used as the next 
;                       value to be divided and the power of 10 is decreased. 
;                       Once the power of 10 reaches 0, the number has been 
;                       converted and stored. 
;
; Arguments:		n (AX) - the 16-bit signed value to convert to a 
;				 decimal string
;		        a (SI) - the address to store the decimal string
; 
; Return Value:         None
;
; Local Variables:      n (AX) - copy of passed binary value to convert
;		        digit (AX)    - computed decimal digit
;		        maxpwr10 (CX) - current power of 10
;		        a (SI) 	      - address of string result from conversion 
;
; Shared Variables:     None 
;
; Global Variables:     None 
;
; Input: 	        None 
;
; Output:               None 
;
; Error Handling:       None 
;
; Algorithms: 	        Repeatedly divide by powers of 10 and get the 
;			remainders 
;
; Data Structures: 	None 
;
; Registers Changed: 	AX, BX, CX, DX, SI
;
; Author: Yuan Ma 
; Last Modified: 11/1/15
;


Dec2String      PROC        NEAR
                PUBLIC      Dec2String

Dec2StringInit:			        ;initialization
	MOV 	BX, AX		        ;BX = arg
	MOV     CX, MAXPWR10            ;start with the largest power of 10 
	CMP     BX, 0    		;sets sign flag 	    	
	JL      NegSign		        ;if sign flag is 1, adds negative sign
	;JGE 	PosSign		        ;if sign flag is 0, adds positive sigN

PosSign:
	MOV     BYTE PTR [SI], '+'      ;writes the positive sign
	JMP 	Dec2StringLoop

NegSign:
	MOV     BYTE PTR [SI], '-'      ;writes the negative sign
	NEG     BX			;changes to positive equivalent
	JMP 	Dec2StringLooP

Dec2StringLoop:				;initializes loop
	INC     SI			;starts at the next digit
	CMP     CX, 0			;check if prw10 > 0
	JLE 	EndDec2StringLoop	;have all digits, done
	;JMP	Dec2StringBody		;else, get the next digit

Dec2StringBody:				;get a digit
	MOV     AX, BX
        MOV     DX, 0			;setup for arg/pwr10
        DIV     CX                      ;digit(AX) = arg/pwr10
	ADD     AL, '0'                 ;gets ASCII code 
        MOV     BYTE PTR [SI], AL       ;convert to ASCII and writes digit
	MOV     BX, DX			;now work with arg = arg MOD pwr10
	MOV     AX, CX 			;setup to update pwr10
	MOV     CX, 10                  ;clears CX
        MOV     DX, 0                   ;clears DX for division 
	DIV     CX
	MOV 	CX, AX			;pwr10 = pwr10/10
	JMP     Dec2StringLoop		;check if more digits

EndDec2StringLoop:
	MOV     BYTE PTR [SI], ASCII_NULL	;writes null 
	RET

Dec2String	ENDP


; Hex2String
;
; Description:		This function is passed a 16-bit unsigned value (n)  
;			and converts it to a hexadecimal value in a string 
;			representation with ASCII characters. The string 
;			has 5 characters (in ASCII) and is 
;			terminated with a <null>. The resulting string is  
;		        stored starting at the memory location indicated 
;			by the passed in address (a). 
;
; Operation:		The function starts with the largest power of 16 
;			possible and loops dividing the number by the 
;			decreasing powers of 16 to get each digit. The 
;			digits are then converted to their ASCII  
;			representations and stored at the passed in memory 
;			location (a). If a digit is greater than 9, it is 
;			converted to an ASCII letter by adding the value for
;                       for ASCII A to get the ASCII code. Ever time a digit 
;			is stored, the remainder of the division is used as 
;                       the next value to be divided and the power of 16 is  
;                       decreased. Once the power of 16 reaches 0, the number  
;                       has been converted and stored. 
;
; Arguments:		n (AX) - the 16-bit unsigned value to convert to a 
;				 hexadecimal string
;			a (SI) - the address to store the hexadecimal string
;
; Return Value:		None
;
; Local Variables: 	n (AX) - copy of passed binary value to convert
;			hexdigit (AX) - computed hexadecimal digit
;			maxpwr16 (CX) - current power of 16
;			a (SI) - string result from conversion 
;
; Shared Variables:	None 
;
; Global Variables:	None 
;
; Input: 		None 
;
; Output: 		None 
;
; Error Handling: 	None 
;
; Algorithms:		Repeatedly divide by powers of 16 and get the 
;			remainders. 
;
; Data Structures: 	None 
;
; Registers Changed:	AX, BX, CX, DX, SI
;
; Author: Yuan Ma
; Last Modified: 11/1/15
;


Hex2String      PROC        NEAR
                PUBLIC      Hex2String


Hex2StringInit:				;initialization
	MOV 	BX, AX			;BX = arg
	MOV     CX, MAXPWR16		;start with the highest power of 16 
	JMP     Hex2StringLoop

Hex2StringLoop:				;iniTIalizes loop
	CMP     CX, 0			;check if prw16 > 0
	JLE 	EndHex2StringLoop	;have all digits, done
	;JMP	Hex2StringBody		;else, get the next digit

Hex2StringBody:				;get a digit
	MOV     AX, BX
        MOV     DX, 0			;setup for arg/pwr16
	DIV     CX			;digit(AX) = arg/pwr16
	CMP     AL, 10			;check if digit needs to be a letter  
	JGE     ConvertLetter		;if greater or equal to 10, convert to letter
	;JMP 	ConvertNumber		;if less than 10, convert to number

ConvertNumber:
	ADD     AL, '0'                 ;gets ASCII code 
        MOV     BYTE PTR [SI], AL       ;convert to ASCII and writes digit
	JMP     Hex2StringBodyCont	;continues loop
    
ConvertLetter:
        SUB     AX, 10                  ;subtracts to get 0 so we can add the full 
                                        ;ASCII value of A 
	ADD     AX, 'A'                 ;gets ASCII code 
        MOV     BYTE PTR [SI], AL       ;converts to ASCII letters and writes digit
	JMP	Hex2StringBodyCont	;continues loop

Hex2StringBodyCont:
	INC     SI		        ;start at next digit
	MOV     BX, DX			;now work with arg = arg MOD pwr16
	MOV     AX, CX 		        ;setup to update pwr16
	MOV     CX, 16		        ;clears CX
        MOV     DX, 0                   ;clears DX for divison 
	DIV     CX
	MOV 	CX, AX			;pwr16 = pwr16/16
	JMP     Hex2StringLoop		;check if more digits

EndHex2StringLoop:
	MOV     BYTE PTR [SI], ASCII_NULL	;writes null 
	RET

Hex2String	ENDP


CODE    ENDS



        END
