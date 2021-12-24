; NAM     RAMP    REV.A  SEPTEMBER 1977
; OPT     NOP,O
; MODEL 68 MUSIC BOARD TEST ROUTINE.
; THIS ROUTINE PRODUCES A TRIANGULAR RAMP OF 319 HZ
; ON A 6800 COMPUTER SYSTEM RUNNING AT 0.89855 MHZ
; NEWTECH COMPUTER SYSTEMS,INC.
MOD68               equ       $8010               ; MUSIC BOARD IN I/O SLOT 4
;                                CAN BY CHANGED FOR OTHER SLOTS
                    org       $0100
RAMP                inca
                    staa      MOD68
                    bra       RAMP
