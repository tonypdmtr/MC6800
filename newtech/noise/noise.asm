; NAM     NOISE   REV.A  SEPTEMBER 1977
; OPT     NOP,O
; COPYRIGHT (C) ALL RIGHTS RESERVED
; NEWTECH COMPUTER SYSTEMS,INC.
;

                    org       $0100
                    ldx       #TBL                ; INIT. ENVELOPE POINTER.
                    stx       TBLP
LOOP2               ldaa      #$02                ; IF ENVELOPE IS
                    ldx       TBLP                ; COMPLETE RETURN.
                    cmpa      0,X
                    beq       EXIT
                    ldx       TBLP                ; ELSE INC. ENV. POINTER.
                    inx
                    stx       TBLP
                    ldaa      DURA                ; OUTPUT N RANDOM VALUES
                    staa      DURAT               ; WHERE N=DURA
LOOP1               ldaa      FREQ                ; DELAY ACCORDING TO
LOOP3               deca                          ; FREQUENCY PARAMETER.
                    bne       LOOP3
                    bsr       RNDM                ; GET RANDOM NUMBER IN A.
                    ldx       TBLP                ; SCALE AMPLITUDE ACCORDING
                    anda      0,X                 ; TO ENV. TABLE.
                    staa      MOD68               ; OUTPUT TO MUSIC BOARD
                    dec       DURAT
                    bne       LOOP1
                    bra       LOOP2               ; PROCESS NEXT AMPLITUDE.

EXIT                jmp       MIKBUG              ; YOUR EXIT MAY DIFFER!

; RANDOM NUMBER GENRATOR. GENERATES 16 BIT
; VALUE IN "NMBER".  RETURNS MOST SIGNIFICANT
; BYTE IN A.
RNDM                ldaa      MSB                 ; EXCLUSIVE-OR SHIFT
                    rora                          ; REGISTER BITS 15,14,12&3.
                    eora      MSB                 ; 15 & 14
                    rora
                    rora
                    eora      MSB                 ; 12
                    rora
                    eora      LSB                 ; 3
                    rora
                    rora
                    anda      #$01                ; MASK BIT 0
                    asl       LSB                 ; SHIFT NUMBER LEFT.
                    rol       MSB                 ; SETTING BIT 0 ACCORDING
                    adda      LSB                 ; TO EXCLUSIVE-OR CALC
                    staa      LSB
                    rts

;
; AMPLITUDE ENVELOPE SPECIFICATION:
TBL                 fcb       $FF,$FF,$FF,$7F,$7F,$3F
                    fcb       $3F,$1F,$0F,$07,$02
;
FREQ                fcb       $30                 ; NOISE BAND PARAMETERS
DURA                fcb       $FF                 ; DURATION PARAMETER.
NMBER               fdb       $01                 ; SHIFT REGISTER
MOD68               equ       $8010               ; MUSIC BOARD I/0 ADDRESS.
MIKBUG              equ       $E0D0
TBLP                rmb       2                   ; ENVELOPE TABLE POINTER
DURAT               rmb       1                   ; TEMPORARY DURATION COUNT.
MSB                 equ       NMBER               ; RANDOM NUMBER ROUTINE.
LSB                 equ       NMBER+1
                    nop
                    end
