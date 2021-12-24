; NAM BLKJCK
;      BLACKJACK REV 1.5 RICK STERLING QUAD EIGHT ELEC. 8/16/76
;
;      NOTE TO USERS, THERE IS A TIMER INCORPORATED IN
;      THIS PROGRAM. IT IS USED TO GENERATE DELAYS DURING
;      PLAY WHEN OPERATING AT 9600 BAUD. THOSE OF YOU WHO
;      ARE USING SLOWER BAUD RATES MAY WISH TO DELETE THIS
;      FUNCTION.
;
;      ADAPTED FOR THE MC3 BY DANIEL TUFVESSON 2013
;
DLRTOT              equ       $309
PLRTOT              equ       $30A
DLRSAV              equ       $305
PLRSAV              equ       $306
PLRACE              equ       $307
DLRACE              equ       $308
SAVEX               equ       $300
DKPNTR              equ       $302
FLAG                equ       $304
RANDNO              equ       $100
TEMP                equ       $104
DECK                equ       $105
                    org       $1000
;      INITIALIZE STACK POINTER PRINT MESSAGE
;      CLEAR SCORE CLEAR CARD MEMORY
START               ldaa      #$39
                    staa      $FCF4
                    lds       #$FF0
                    ldx       #SHUF
                    jsr       PRINT
                    ldx       #$3030
                    stx       PLRSCR
                    stx       DLRSCR
                    clr       DLRTOT
                    clr       PLRTOT
;      JUMP TO SHUFFLE ROUTINE REPEAT UNTIL
;      KEY PUSHED
LOOPC               jsr       SHUFLE+6
                    ldaa      $FCF4
                    asra
                    bcc       LOOPC
                    ldaa      $FCF5
                    ldx       #DECK
                    stx       DKPNTR
;      PLAY STARTS HERE, ALL TEMP REGISTERS ARE
;      CLEARED
START1              clr       FLAG
                    bsr       SCORE
                    clr       PLRACE
                    clr       DLRACE
                    clr       DLRSAV
                    clr       PLRSAV
                    jsr       TIME
                    ldx       #OVRLY
                    jsr       PRINT
START2              jsr       DEAL
                    jsr       SCALE
                    jsr       PLRCRD
                    jsr       OUTCD
                    ldx       #SPACE
                    jsr       PRINT
                    jsr       DEAL
                    jsr       SCALE
                    jsr       DLRCRD
                    ldab      FLAG
                    beq       FIRST
                    staa      TEMP
                    bra       SECOND

FIRST               jsr       OUTCD
SECOND              ldaa      #$A
                    jsr       COUT
                    ldaa      #$D
                    jsr       COUT
                    inc       FLAG
                    ldaa      FLAG
                    cmpa      #2
                    bne       START2
                    ldaa      PLRSAV
                    cmpa      #22
                    bne       NEX1
                    ldaa      #12
                    staa      PLRSAV
                    dec       PLRACE
NEX1                cmpa      #21
                    bne       CKDLR
                    ldx       #PLRBKJ
                    jsr       PRINT
                    inc       PLRTOT
                    bra       START1

; *COUT   EQU * CHARATOR OUTPUT ROUTINE
;       PSHB SAVE B REGISTER
;       LDAB $FCF4 CONTROL REG
;       ASRB
;       ASRB
;       BCC COUT+1
;       STAA $FCF5 OUTPUT CHAR
;       CLRB
;       DECB
;       BNE *-1
;       PULB RESTORE B REG
;       RTS
SCORE               tst       DLRTOT
                    beq       NEXT1
                    clr       DLRTOT
                    ldx       #DLRSCR
                    bsr       BCDADD
NEXT1               tst       PLRTOT
                    beq       OUT7
                    clr       PLRTOT
                    ldx       #PLRSCR
                    bsr       BCDADD
OUT7                rts

BCDADD              ldaa      1,X
                    ldab      ,X
                    anda      #$0F
                    andb      #$0F
                    aslb
                    aslb
                    aslb
                    aslb
                    aba
                    adda      #1
                    daa
                    tab
                    andb      #$F
                    addb      #$30
                    stab      1,X
                    lsra
                    lsra
                    lsra
                    lsra
                    adda      #$30
                    staa      ,X
                    rts

TIME                clra
TIMEL               ldx       #$0000              ; THIS VALUE MAY BE ADJ. FOR DIFFERENT TIMEOUT
LOOPT               inx
                    bne       LOOPT
                    inca
                    cmpa      #20                 ; THIS VALUE MAY BE ADJ. FOR DIFFERENT TIMEOUT
                    bne       TIMEL
                    rts

CKDLR               ldaa      DLRSAV
                    cmpa      #22
                    bne       NEX2
                    ldaa      #12
                    staa      DLRSAV
                    dec       DLRACE
NEX2                cmpa      #21
                    bne       HIT1
                    ldx       #DWNCDM
                    jsr       PRINT
                    ldaa      TEMP
                    jsr       OUTCD
                    ldx       #DLRBKJ
                    jsr       PRINT
                    inc       DLRTOT
                    jmp       START1

HIT1                ldx       #HITMES
                    jsr       PRINT
                    jsr       CIN
                    cmpa      #$31
                    beq       HIT2
                    bra       DLRNX2              ; former DLRNXT

HIT2                jsr       DEAL
                    ldx       #DWNCDM             ; reconstructed line
                    jsr       PRINT
                    ldaa      TEMP
                    bsr       OUTCD
                    ldaa      #$A
                    jsr       COUT
                    ldaa      #$D
                    jsr       COUT
DLRNX2              ldaa      DLRSAV
                    cmpa      #16
                    bhi       STOP
                    ldx       #SPC18
                    jsr       PRINT
                    jsr       DEAL
                    bsr       SCALE
                    jsr       DLRCRD
                    bsr       OUTCD
                    bra       DLRNX2

SCALE               tab
                    andb      #$F
                    incb
                    cmpb      #1
                    beq       ACE
                    cmpb      #10
                    bhi       TENS
                    rts

ACE                 ldab      #11
                    rts

TENS                ldab      #10
                    rts

STOP                cmpa      #21
                    bhi       BUST2
                    cmpa      PLRSAV
                    bhi       DLRWIN
                    beq       PUSH
PLRW                ldx       #PLRWIN
                    jsr       PRINT
                    inc       PLRTOT
                    bra       OUT5

DLRWIN              ldx       #DLRWMS
                    jsr       PRINT
                    inc       DLRTOT
OUT5                jmp       START1

BUST2               ldaa      DLRACE
                    beq       BUST2A
                    dec       DLRACE
                    ldaa      DLRSAV
                    suba      #10
                    staa      DLRSAV
                    bra       DLRNX2

BUST2A              ldx       #BUST
                    jsr       PRINT
                    bra       PLRW

PUSH                ldx       #PUSHM
                    jsr       PRINT
                    bra       OUT5

                    jmp       START1

OUTCD               psha
                    anda      #$F
                    tab
                    aba
                    aba
                    aba
                    aba
                    clrb
                    ldx       #CARD
                    stx       SAVEX
                    adda      SAVEX+1
                    adcb      SAVEX
                    staa      SAVEX+1
                    stab      SAVEX
                    ldx       SAVEX
                    ldab      #5
                    bsr       PRINK
                    ldaa      #'/'
                    jsr       COUT
                    pula
                    anda      #$30
                    lsra
                    ldx       #SUIT
                    clrb
                    stx       SAVEX
                    adda      SAVEX+1
                    adcb      SAVEX
                    staa      SAVEX+1
                    stab      SAVEX
                    ldx       SAVEX
                    ldab      #8
                    bsr       PRINK
                    rts

PRINK               ldaa      ,X
                    jsr       COUT
                    inx
                    decb
                    bne       PRINK
                    rts

DEAL                ldx       DKPNTR
                    cpx       #DECK+52
                    bne       DECKOK
                    ldx       #SHUF2
                    jsr       PRINT
                    ins
                    ins
                    jmp       LOOPC

DECKOK              ldx       DKPNTR
                    ldaa      ,X
                    inc       DKPNTR+1
                    rts

PLRCRD              cmpb      #11
                    bne       PLRCR1
                    inc       PLRACE
PLRCR1              addb      PLRSAV
                    stab      PLRSAV
                    rts

DLRCRD              cmpb      #11
                    bne       DLRCR1
                    inc       DLRACE
DLRCR1              addb      DLRSAV
                    stab      DLRSAV
                    rts

SPACE               fcb       $20                 ; reconstructed line
                    fcb       $4
CARD                fcc       '  ACE  TWOTHREE FOUR FIVE  SIXSEVEN'
                    fcc       'EIGHT NINE  TEN JACKQUEEN KING'
SUIT                fcc       'SPADES  HEARTS  CLUBS   DIAMONDS'
HITMES              fcb       $A,$D
                    fcb       $3F,$1A,$4
BUST                fcb       $A,$D,$7
                    fcc       'BUST'
                    fcb       $4
SPC18               fcb       $A,$D
                    fcc       'DEALER DRAWS A    '
                    fcb       $4
PLRWIN              fcb       $A,$D,$7
                    fcc       'YOU WIN'
                    fcb       $A,$D,$4
DLRWMS              fcb       $A,$D,$7
                    fcc       'DEALER WINS'
                    fcb       $A,$D,$4
PLRBKJ              fcb       $A,$D
                    fcc       'B L A C K J A C K    Y O U   W I N ! !'
                    fcb       $4
DLRBKJ              fcb       $A,$D
                    fcc       'B L A C K J A C K    I    W I N ! ! !'
                    fcb       $4
PUSHM               fcb       $A,$D,$7
                    fcc       'PUSH'
                    fcb       $4
DWNCDM              fcb       $A,$D
                    fcc       'MY DOWN CARD WAS  '
                    fcb       $4
; *CIN    EQU *
;       LDAA $FCF4
;       ASRA
;       BCC CIN
;       LDAA $FCF5
;       ANDA #$7F
;       RTS
;      THIS SUB-ROUTINE GENERATES A 52 CARD DECK
SHUFLE              ldx       #SHUF2
                    jsr       PRINT
                    jsr       INIT
LOOP4               bsr       RANDOM
                    bsr       CHECK
                    bsr       LOAD
                    bcc       LOOP4
                    rts

;      THIS IS A 32 BIT RANDOM (PSEUDO) NUMBER GENERATOR
;      4.29X10^9 STATES
RANDOM              ldaa      RANDNO+3
                    oraa      RANDNO+2
                    oraa      RANDNO+1
                    oraa      RANDNO
                    bne       SKIP                ; IF ZERO ENTER ALL 1'S
                    com       RANDNO+3
SKIP                ldaa      RANDNO+3
                    lsra
                    lsra
                    lsra
                    eora      RANDNO+3
                    asra
                    asra
                    ror       RANDNO
                    ror       RANDNO+1
                    ror       RANDNO+2
                    ror       RANDNO+3
                    rts

;      MODULO 52 MASK
CHECK               ldaa      RANDNO
                    eora      RANDNO+1
                    eora      RANDNO+2
                    eora      RANDNO+3
                    tab
                    anda      #$F
                    cmpa      #12
                    ble       OK
                    suba      #13
OK                  andb      #$30
                    aba
                    rts

;      SUBROUTINE TO LOAD DECK FROM RANDOM # GEN.
LOAD                ldx       #DECK
LOOP2               ldab      ,X
                    bmi       ENT
                    cba
                    beq       OUTNG
                    inx
                    cpx       #DECK+52
                    beq       OUTOK
                    bra       LOOP2

ENT                 staa      ,X
OUTNG               cpx       #DECK+51
                    beq       OUTOK
                    clc
                    rts

OUTOK               sec
                    rts

SHUF                fcb       $06,06,$0C,0,0,0,0,0,$A,$D
                    fcc       'THIS IS MOTOROLA BLACKJACK......'
                    fcc       '.........TRY YOUR LUCK.....!'
                    fcb       $D,$A
                    fcb       6,7
                    fcc       'DEALER MUST HIT ON 16 OR LESS AND'
                    fcc       ' STAND ON 17.'
                    fcb       $D,$A,0,0,0,0,0
                    fcc       'ACES COUNT 1 OR 11 , FACE CARDS'
                    fcc       ' COUNT TEN.'
                    fcb       $D,$A,0,0,0,0,0
                    fcc       'BLACKJACK WITH ACE AND A FACE CARD'
                    fcc       ' OR TEN'
                    fcb       $D,$A,0,0,0,0,0
                    fcc       'TIES COUNT AS A PUSH NO WINNER.'
                    fcb       $D,$A,0,0,0,0,0
                    fcc       'DURING PLAY TYPE 1 FOR A HIT,0 TO'
                    fcc       ' STAND PAT'
                    fcb       $D,$A,0,0,0,0,0
SHUF2               fcb       $D,$A,6,2,0,0
                    fcc       'SHUFFLE NOW IN PROGRESS.............'
                    fcb       6,1
                    fcb       $D,$A,0,0,0,0,0
                    fcb       $4
PRINT               equ       *
                    ldaa      ,X
                    cmpa      #$4
                    bne       NOTEND
                    rts

NOTEND              bsr       COUT
                    inx
                    bra       PRINT

INIT                equ       *                   ; INITIALIZE DECK FOR LOAD
;      ROUTINE , ALL CARDS EQUAL FF
                    ldx       #DECK
                    ldaa      #$FF
LOOP7               staa      ,X
                    inx
                    cpx       #DECK+52
                    bne       LOOP7
                    rts

OVRLY               fcb       $0C,0,0,0
                    fcb       $D,$A,0,0,0,0,0
                    fcb       13
                    fcc       'PLAYER'
                    fcb       9
                    fcc       'DEALER'
                    fcb       $D,$A,8,8,8
                    fcb       6
PLRSCR              rmb       2
                    org       *
                    fcb       9
DLRSCR              rmb       2
                    org       *
                    fcb       $D,$A,$A,$4
CIN                 jmp       INCHAR

COUT                jmp       OUTCHAR

OUTCHAR             equ       $E1D1
INCHAR              equ       $E1AC
                    end
