;      "MISSIONARIES AND CANNIBALS" GAME
;      ORIGINAL PROGRAM BY PHILIP N. THEURER
;      REV. 1.0 BY MOTOROLA MICROSYSTEMS NOVEMBER 25, 1976
;      STARTING ADDRESS: 0100

                    org       $0080
MISA                rmb       1
MISB                rmb       1
CANA                rmb       1
CANB                rmb       1
MISAT               rmb       1
MISBT               rmb       1
CANAT               rmb       1
CANBT               rmb       1
TRIP                rmb       1
COUNT1              rmb       1
COUNT2              rmb       1
COUNT3              rmb       1
COUNT4              rmb       1

INCHNP              equ       $E1AC
OUTCH               equ       $E1D1
PDATA               equ       $e07e
RETURN              equ       $e0d0

                    org       $0100
BEGIN               ldx       #INTRO              ; PRINT INSTRUCTIONS
BEGIN1              jsr       PDATA
BEGIN2              ldaa      #3
                    staa      MISA
                    staa      CANA
                    clra
                    staa      MISB
                    staa      CANB
                    staa      TRIP

GAME                jsr       P2SP
                    jsr       P2SP
                    jmp       RIVER

CONTIN              jsr       PCRLF
                    tst       TRIP
                    bne       RGHTAR
                    ldaa      #'<'
                    bra       F1

RGHTAR              ldaa      #'>'
F1                  jsr       OUTCH
                    jsr       OUTCH
                    ldx       MISA                ; STORE LAST LINE VALUES
                    stx       MISAT
                    ldx       CANA
                    stx       CANAT
                    ldaa      #2
                    staa      COUNT2
                    staa      COUNT3
LOOP                jsr       INCHNP
                    cmpa      #'M'
                    beq       F2
                    cmpa      #'C'
                    beq       F3
                    cmpa      #'E'
                    beq       F4
                    ldx       #CR2LF
                    cmpa      #'R'
                    beq       BEGIN1
                    cmpa      #'I'
                    beq       BEGIN
                    cmpa      #'X'
                    bne       *+5
                    jmp       RETURN

                    bra       ERRORR

F4                  dec       COUNT3
                    bne       F5
                    bra       ERRORR

F2                  tst       TRIP                ; MOVE MISSIONARY
                    bne       ODDMIS
                    tst       MISB
                    bne       MISBNZ
                    bra       ERRORR              ; NO MISSIONARY B, ERROR

MISBNZ              dec       MISB                ; MOVE LEFT IF TRIP FLAG 0
                    inc       MISA
                    bra       F5

ODDMIS              tst       MISA
                    bne       MISANZ
ERRORR              jmp       ERROR               ; NO MISSIONARY A, ERROR

MISANZ              dec       MISA                ; MOVE RIGHT IF TRIP FLAG NOT 0
                    inc       MISB
                    bra       F5

F3                  tst       TRIP                ; MOVE CANNIBAL
                    bne       ODDCAN
                    tst       CANB
                    bne       CANBNZ
                    bra       ERRORR              ; NO CANNIBAL B, ERROR

CANBNZ              dec       CANB                ; MOVE LEFT
                    inc       CANA
                    bra       F5

ODDCAN              tst       CANA
                    bne       CANANZ
                    bra       ERRORR              ; NO CANNIBAL A, ERROR

CANANZ              dec       CANA                ; MOVE RIGHT
                    inc       CANB
F5                  dec       COUNT2
                    bne       LOOP
RIVER               ldaa      #10
                    staa      COUNT1
                    ldaa      #$20
SLOOP               bsr       OUTCHR
                    dec       COUNT1
                    bne       SLOOP
                    ldaa      CANA                ; PRNT CANA VALUE
                    bsr       ONC
                    ldaa      #'C'
                    bsr       OC2S
                    ldaa      MISA                ; PRINT MISA VALUE
                    bsr       ONC
                    ldaa      #'M'
                    bsr       OC2S
                    ldaa      #16
                    staa      COUNT1
F6                  tst       TRIP
                    bne       RGHTPT
                    ldaa      #'<'
                    bsr       OUTCHR
                    bra       F7

OUTCHR              jmp       OUTCH

RGHTPT              ldaa      #'>'
                    bsr       OUTCHR
F7                  dec       COUNT1
                    bne       F6
                    bsr       P2SP
                    ldaa      CANB                ; PRINT CANB VALUE
                    bsr       ONC
                    ldaa      #'C'
                    bsr       OC2S
                    ldaa      MISB                ; PRINT MISB VALUE
                    bsr       ONC
                    ldaa      #'M'
                    bsr       OUTCHR
                    tst       MISA                ; MISA AND CANA = 0???
                    bne       F9
                    tst       CANA
                    bne       F9
                    bra       CONGTR              ; YES WIN

F9                  ldaa      CANA                ; IS CANA > MISA?
                    cmpa      MISA
                    ble       F10
                    tst       MISA                ; MISA NOT 0?
                    beq       F10
                    bra       BURN                ; YES LOSE

F10                 ldaa      CANB                ; IS CANB > MISB
                    cmpa      MISB
                    ble       F11
                    tst       MISB                ; MISB NOT 0?
                    beq       F11
                    bra       BURN                ; YES LOSE

F11                 com       TRIP                ; COMPLEMENT TRIP FLAG
                    jmp       CONTIN

; *SUBROUTINES

OC2S                bsr       OUTCHR
P2SP                jsr       PSPACE
;       JSR PSPACE
                    rts

ONC                 oraa      #$30
                    bsr       OUTCHR
                    rts

ERROR               ldaa      #'?'
                    bsr       OUTCHR
                    ldaa      #7                  ; BELL
                    bsr       OUTCHR
                    ldx       MISAT               ; RESTORE VALUE TO
                    stx       MISA                ; PREVIOUS TRIP
                    ldx       CANAT
                    stx       CANA
                    jmp       CONTIN

BURN                ldx       #LOSE               ; PRINT LOSE MSG
                    jsr       PDATA
                    jmp       BEGIN2

CONGTR              ldx       #WIN                ; PRINT WIN MSG
                    jsr       PDATA
WAIT                jsr       INCHNP
                    cmpa      #'I'
                    bne       WAIT
                    jmp       BEGIN

; *MESSAGES

INTRO               fcb       $D,$A,$A,$A,$A
                    fcc       "                   MISSIONARIES AND "
                    fcc       "CANNIBALS"
                    fcb       $D,$A,$A
                    fcc       "             ORIGINAL PROGRAM BY "
                    fcc       "PHILIP N. THEURER"
                    fcb       $D,$A
                    fcc       "            MOTOROLA MICROSYSTEMS "
                    fcc       "NOVEMBER 25, 1976"
                    fcb       $D,$A,$A,$A
                    fcc       "   THREE MISSIONARIES AND THREE "
                    fcc       "CANNIBALS ARE TRAVELING TOGETHER"
                    fcb       $D,$A
                    fcc       "AND COME UPON THE GREAT UGAHNA "
                    fcc       "RIVER WHICH THEY MUST CROSS.  UN-"
                    fcb       $D,$A
                    fcc       "FORTUNATELY THEY HAVE ONLY ONE B"
                    fcc       "OAT AND IT CAN ONLY HOLD TWO PEOPLE."
                    fcb       $D,$A
                    fcc       "TO FURTHER COMPLICATE MATTERS,"
                    fcc       " THE CANNIBALS ARE UNCIVILIZED AND"
                    fcb       $D,$A
                    fcc       "WILL EAT THE MISSIONARIES IF AT"
                    fcc       " ANY TIME THEY OUTNUMBER THEM."
                    fcb       $D,$A
                    fcc       "   YOUR MISSION IS TO MOVE ALL THE "
                    fcc       "CANNIBALS AND MISSIONARIES ACROSS"
                    fcb       $D,$A
                    fcc       "THE RIVER WITHOUT HAVING ANY OF "
                    fcc       "THE MISSIONARIES DEVOURED BY THE"
                    fcb       $D,$A
                    fcc       "CANNIBALS.  ARROWS WILL SHOW WHICH"
                    fcc       " WAY THE BOAT IS READY TO SAIL."
                    fcb       $D,$A
                    fcc       "TYPE AN 'M' TO PLACE A MISSIONARY"
                    fcc       " IN THE BOAT, A 'C' TO PLACE A"
                    fcb       $D,$A
                    fcc       "CANNIBAL IN THE BOAT, OR AN 'E' TO"
                    fcc       " LEAVE AN EMPTY SEAT.  TO RESTART"
                    fcb       $D,$A
                    fcc       "THE GAME TYPE AN 'R', OR TO HAVE "
                    fcc       "THE INSTRUCTIONS REPRINTED TYPE AN 'I'"
                    fcb       $D,$A
                    fcc       "TO EXIT PROGRAM TYPE AN 'X'"
                    fcb       $D,$A
                    fcc       "GOOD LUCK!"

CR2LF               fcb       $D,$A,$A,4
WIN                 fcb       $D,$A,$A
                    fcc       "   CONGRATULATIONS SMART ALEC. "
                    fcc       " CANNIBALS ALL OVER THE WORLD WILL"
                    fcb       $D,$A
                    fcc       "STARVE BECAUSE OF YOU!!!"
                    fcb       $D,$A,$A,4
LOSE                fcb       $D,$A,$A
                    fcc       "   I CAN SEE THAT YOU ARE DOING"
                    fcc       " YOUR PART FOR ZERO POPULATION GROWTH."
                    fcb       $D,$A,$A
                    fcc       "BURP!!!!"
                    fcb       7,$D,$A,$A,4

PCRLF               ldx       #CR2LF
                    jsr       PDATA
                    rts

PSPACE              ldaa      #$20
                    jsr       OUTCH
                    rts

                    end
