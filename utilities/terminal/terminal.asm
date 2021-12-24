;***********************************************
;    RS-232 COMMUNICATIONS ROUTINE FOR THE    *
;           SWTP  6800  COMPUTER              *
;                                             *
;***********************************************

; DEFINITIONS:

P1CR                equ       $8004               ; PORT 1 CONTROL AND STATUS REGISTERS
P1DR                equ       $8005               ; PORT 1 DATA REGISTER
P1SET               equ       $55                 ; PORT 1 SET TO 8N1
P2CR                equ       $8008               ; PORT 2 CONTROL AND STATUS REGISTERS
P2DR                equ       $8009               ; PORT 2 DATA REGISTER
P2SET               equ       $55                 ; PORT 2 SET TO 8N1

DELAY               equ       $E2C2

; PROGRAM:

                    org       $100

INIT                ldab      #3                  ; TO RESET ACIA'S
                    ldx       #P1CR               ; SET INDEX TO PORT 1
                    stab      0,X                 ; RESET PORT 1
                    stab      P2CR-P1CR,X         ; RESET PORT 2
                    ldaa      #P1SET              ; TO CONFIGURE ACIA'S TO 8B+1SB
                    staa      0,X                 ; CONFIGURE PORT 1
                    staa      P2CR-P1CR,X         ; CONFIGURE PORT 2
R1                  ldaa      #1                  ; PREPARE TO CHECK PORT 1 FOR DATA
                    anda      0,X                 ; ANY DATA FROM PORT 1?
                    beq       R2                  ; IF NOT, CHECK PORT 2
                    ldaa      1,X                 ; IF SO, LOAD DATA IN ACC A
                    cmpa      #$1F                ; IS IT THE BREAK (^_) CHARACTER?
                    beq       BRK                 ; IF SO, GO BREAK PORT 2
T2                  ldab      #2                  ; IF NOT, PREPARE TO TRANSMIT
                    andb      P2CR-P1CR,X         ; IS PORT 2 READY TO SEND?
                    beq       T2                  ; IF NOT, CHECK AGAIN
                    staa      P2DR-P1CR,X         ; IF SO, SEND DATA OUT PORT 2
                    bra       R1                  ; AND GO CHECK PORT 1 FOR MORE DATA

R2                  ldaa      #1                  ; PREPARE TO CHECK PORT 2 FOR DATA
                    anda      P2CR-P1CR,X         ; ANY DATA FROM PORT 2?
                    beq       R1                  ; IF NOT, CHECK PORT 1
                    ldaa      P2DR-P1CR,X         ; IF SO, LOAD DATA IN ACC A
T1                  ldab      #2                  ; PREPARE TO TRANSMIT
                    andb      0,X                 ; IS PORT 1 READY TO TRANSMIT?
                    beq       T1                  ; IF NOT, CHECK AGAIN
                    staa      1,X                 ; IF SO, SEND DATA OUT PORT 1
                    bra       R2                  ; AND GO CHECK PORT 2 FOR MORE DATA

BRK                 ldaa      #$61                ; PREPARE TO BREAK PORT 2
                    staa      P2CR-P1CR,X         ; BREAK PORT 2
                    jsr       DELAY               ; WAIT A BIT . . .
                    bra       INIT                ; AND START OVER

                    end
