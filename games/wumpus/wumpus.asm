;****************************************************
;       SUPER-WUMPUS
; BY JACK EMMERICHS 1978
; MC6800 ASSEMBLY
;****************************************************

                    org       $0100
                    jmp       START

ARRAY               rmb       5                   ; ROOM SEQUENCE
ARRAYNO             rmb       2                   ; ARRAY POINTER
AWAKE               rmb       1                   ; AWAKE SW
BLANK               equ       $20                 ; BLANK SPACE
COUNT               rmb       1                   ; GENERAL COUNTER
CR                  equ       $0D                 ; CARRIAGE RETURN
CURRENT             rmb       1                   ; CURRENT HUNTER LOCATION
DAYS                rmb       1                   ; DAYS LEFT
DELIM               rmb       1                   ; END OF INPUT DELIMITER
END                 rmb       1                   ; GAME OVER SW
EOT                 equ       4                   ; END OF STRING
LF                  equ       $0A                 ; LINE FEED
OLDROOM             rmb       1                   ; SAVE ROOM NO
ROCKS               rmb       1                   ; ROCK COUNTER
ROOMNO              rmb       1                   ; GENERAL ROOM NO
ROOMTLY             rmb       1                   ; GENERAL ROOM COUNTER
SAVX                rmb       2                   ; SAVE IX
SHOTS               rmb       1                   ; NO OF SHOTS LEFT
STOP                rmb       1                   ; STOP ARROW SW
STUF                rmb       4                   ; RANDOM STUPH FOR RAMDOM NUMBER GENERATOR
SYMBOL              rmb       1                   ; GENERAL SYMBOL HOLDER
TEST                rmb       1                   ; TEST ROOM FOR SHOOTING SEQUENCE
WILD                rmb       1                   ; WILD ARROW IND
WUMPROOM            rmb       1                   ; CURRENT WUMPUS LOCATION
                    org       *
;**SKIP
;
;
ROOMS               fcb       2,5,6,0
                    fcb       1,3,7,0
                    fcb       2,4,8,0
                    fcb       3,5,9,0
                    fcb       1,4,10,0
                    fcb       1,11,12,0
                    fcb       2,12,13,0
                    fcb       3,13,14,0
                    fcb       4,14,15,0
                    fcb       5,11,15,0
                    fcb       6,10,17,0
                    fcb       6,7,18,0
                    fcb       7,8,19,0
                    fcb       8,9,20,0
                    fcb       9,10,16,0
                    fcb       15,17,20,0
                    fcb       11,16,18,0
                    fcb       12,17,19,0
                    fcb       13,18,20,0
                    fcb       14,16,19,0
;
TRANS               fcb       1,1
                    fcb       2,2
                    fcb       3,3
                    fcb       4,4
                    fcb       5,5
                    fcb       6,6
                    fcb       7,7
                    fcb       8,8
                    fcb       9,9
                    fcb       10,10
                    fcb       11,11
                    fcb       12,12
                    fcb       13,13
                    fcb       14,14
                    fcb       15,15
                    fcb       16,16
                    fcb       17,17
                    fcb       18,18
                    fcb       19,19
                    fcb       20,20
;
;
PROB1               fcb       33                  ; ARROW AMONG BATS WILL HIT ONE
PROB2               fcb       33                  ; ARROW OVER PIT WILL FALL IN
PROB3               fcb       20                  ; ARROW HITTING HUNTER WONT KILL
PROB4               fcb       05                  ; BAT MIGRATION
                    fcb       10                  ; FIND THROWING ROCK
                    fcb       05                  ; RATS EAT SUPPLIES
                    fcb       05                  ; ROCK SLIDE
                    fcb       08                  ; FIND MORE SUPPLIES
PROB8               fcb       33                  ; THROW ROCK WILL KILL WUMPUS
;
;
;
;                   org       $0200
START               jmp       MAIN

;**SKIP
;
;
CRLF                fcb       CR,LF,0,EOT
TX01                fcb       CR,LF,LF,0
                    fcc       `CURRENT ROOM IS:`
                    fcb       EOT
TX02                fcb       CR,LF,0,0
                    fcc       `TUNNELS LEAD TO:`
                    fcb       EOT
TX03                fcb       CR,LF,LF,0
                    fcc       `MOVE OR SHOOT? `
                    fcb       EOT
TX04                fcc       ` WHERE? `
                    fcb       EOT
TX05                fcb       CR,LF,0,0
                    fcc       `I HEAR WINGS`
                    fcb       EOT
TX06                fcb       CR,LF,0,0
                    fcc       `I FEEL A DRAFT`
                    fcb       EOT
TX07                fcb       CR,LF,0,0
                    fcc       `I SMELL A WUMPUS!`
                    fcb       EOT
;**SKIP
TX08                fcb       CR,LF,0,0
                    fcc       `YOU HAVE RUN OUT OF SUPPLIES AND WILL STARVE`
                    fcb       EOT
TX09                fcb       CR,LF,0,0
                    fcc       `YYYIIIIEEEEEE.... YOU FELL INTO A PIT`
                    fcb       EOT
TX10                fcb       CR,LF,0,0
                    fcc       `AARRRRGGGGHHH.... YOU HAVE SHOT YOURSELF`
                    fcb       EOT
;**SKIP
TX11                fcb       CR,LF,0,0
                    fcc       `C H O M P  CRUNCH MUNCH GULP!`
                    fcb       CR,LF,0,0
                    fcc       `THE WUMPUS GOT YOU!`
                    fcb       EOT
TX12                fcb       CR,LF,0,0
                    fcc       `YOU LOOSE (HEE HEE)`
                    fcb       EOT
TX13                fcb       CR,LF,0,0
                    fcc       `B O N K ! `
                    fcb       EOT
TX14                fcb       CR,LF,0,0
                    fcc       `GOOD FOR YOU! - YOU GOT HIM`
                    fcb       EOT
TX15                fcc       ` WITH A ROCK!`
                    fcb       EOT
;**SKIP
TX16                fcb       CR,LF,0,0
                    fcc       `SUPPLIES WILL LAST `
                    fcb       EOT
TX17                fcc       ` DAYS`
                    fcb       EOT
TX18                fcb       CR,LF,0,0
                    fcc       `BAT MIGRATION - HIDE AND WAIT`
                    fcb       CR,LF,0,0
                    fcc       `  ONE COLONY OF BATS HAS MOVED`
                    fcb       EOT
TX19                fcb       CR,LF,0,0
                    fcc       `NICE GOING, YOU SHOT YOURSELF`
                    fcb       CR,LF,0,0
                    fcc       `  AND HAVE USED MEDICAL SUPPLIES`
                    fcb       EOT
TX20                fcb       CR,LF,0,0
                    fcc       `WHOOOPS! BATS RELOCATE YOU`
                    fcb       CR,LF,0,0
                    fcc       `  YOU LOST SOME SUPPLIES`
                    fcb       EOT
;**SKIP
TX21                fcb       CR,LF,0,0
                    fcc       `THUD! YOU ONLY DAZED HIM`
                    fcb       EOT
TX22                fcb       CR,LF,0,0
                    fcc       `MISSED`
                    fcb       EOT
TX23                fcb       CR,LF,0,0
                    fcc       `LOOK, YOU FOUND `
                    fcb       EOT
TX24                fcc       `A THROWING ROCK`
                    fcb       EOT
TX25                fcc       `SUPPLIES`
                    fcb       CR,LF,0,0
                    fcc       `  FROM ANOTHER (EATEN) HUNTER`
                    fcb       EOT
;**SKIP
TX26                fcb       CR,LF,0,0
                    fcc       `RATS EAT HALF YOUR SUPPLIES`
                    fcb       EOT
TX27                fcb       CR,LF,0,0
                    fcc       `ROCK SLIDE! A TUNNEL IS CLOSED`
                    fcb       EOT
TX28                fcc       `WATCH IT!`
                    fcb       EOT
TX29                fcc       ` - THE WUMPUS IS AWAKE`
                    fcb       EOT
TX30                fcb       CR,LF,0,0
                    fcc       `WILD SHOT - `
                    fcb       EOT
TX31                fcb       CR,LF,0,0
                    fcc       `BIG DEAL, YOU SHOT A BAT`
                    fcb       EOT
;**SKIP
TX32                fcb       CR,LF,0,0
                    fcc       `A DOWNDRAFT HAS SUCKED THE ARROW INTO A PIT`
                    fcb       EOT
TX33                fcb       CR,LF,0,0
                    fcc       `YOU JUST STEPPED ON HIS FLIPPER!`
                    fcb       EOT
TX34                fcb       CR,LF,LF,0
                    fcc       `DO YOU WANT TO PLAY AGAIN? `
                    fcb       EOT
TX35                fcb       CR,LF,0,0
                    fcc       `DO YOU WANT TO SEE A CAVE MAP? `
                    fcb       EOT
;**SKIP
TX36                fcb       CR,LF,0,0
                    fcc       `WELCOME TO THE WUMPUS CAVES,`
                    fcb       CR,LF,0,0
                    fcc       `DO YOU WISH INSTRUCTIONS? `
                    fcb       EOT
WHAT                fcb       LF
                    fcc       ` WHAT? `
                    fcb       EOT
TX37                fcb       CR,LF,0,0
                    fcc       `THANK YOU, CAME AGAIN SOMETIME.`
                    fcb       EOT
;**SKIP
TX38                fcb       CR,LF,0,0
                    fcc       `SHOOT WHAT?`
                    fcb       CR,LF,0,0
                    fcc       `YOU HAVE NO ARROWS OR ROCKS`
                    fcb       EOT
TX39                fcc       ` - YOU CAN'T GET THERE FROM HERE`
                    fcb       EOT
TX40                fcb       CR,LF,LF,0
                    fcc       `ROOM    TUNNELS      ITEM`
                    fcb       LF,EOT
;**SKIP
TX99                fcb       CR,LF,0,0
                    fcc       `THIS CAVE HAS 20 INTERCONNECTED ROOMS`
                    fcb       CR,LF,0,0
                    fcc       `THE WUMPUS IS ASLEEP IN ONE AND MUST BE KILLED`
                    fcb       CR,LF,0,0
                    fcc       `IF HE (?) FINDS YOU HE WILL EAT YOU`
                    fcb       CR,LF,0,0
;**SKIP
                    fcc       `IF YOU WAKE HIM UP HE MAY MOVE`
                    fcb       CR,LF,0,0
                    fcc       `LOOK OUT FOR BATS AND PITS`
                    fcb       CR,LF,0,0
                    fcc       `WARNINGS ARE FOR HAZARDS 1 ROOM AWAY`
                    fcb       CR,LF,0,0
                    fcc       `YOU HAVE 5 ARROWS AND A LIMITED STORE OF SUPPLIES`
                    fcb       CR,LF,0,0
;**SKIP
                    fcc       `EACH SHOT MAY GO THROUGH 1 TO 3 CONNECTED ROOMS`
                    fcb       CR,LF,0,0
                    fcc       `YOU MAY MAKE 1 MOVE OR SHOT EACH DAY`
                    fcb       CR,LF,0,0
                    fcc       `WHEN YOU ARE OUT OF ARROWS LOOK FOR ROCKS`
                    fcb       CR,LF,0,0
;**SKIP
                    fcc       `  WHICH CAN BE THROWN INTO ONLY 1 ROOM`
                    fcb       CR,LF,0,0
                    fcc       `  AND MAY KILL THE WUMPUS`
                    fcb       CR,LF,0,0
                    fcc       `         GOOD LUCK`
                    fcb       CR,LF,LF,EOT
;**SKIP
;
;
PSHX                stx       SAVX                ; SAVE IX
                    tsx                           ; POINT IX
                    dex
                    dex
                    lds       2,X                 ; MOVE RTS ADDR
                    sts       ,X
                    lds       SAVX                ; INSERT IX
                    sts       2,X                 ; RESTORE STACK
                    txs
                    ldx       2,X                 ; RETORE IX
                    rts

;
;
;
PULX                tsx                           ; POINT IX
                    dex
                    dex
                    lds       4,X                 ; REMOVE IX VALUE
                    sts       ,X
                    lds       2,X                 ; MOVE UP RTS ADDR
                    sts       4,X                 ; RESTORE STACK
                    txs
                    ins
                    ins
                    ins
                    ins
                    ldx       ,X                  ; LOAD IX
                    rts

;**SKIP
;
;
ROOMIN              bsr       PSHX                ; SAVE IX
                    ldab      #3
                    stab      COUNT
                    clr       ROOMNO
IN1                 jsr       CHARIN              ; GET CHARACTER
                    cmpa      #BLANK              ; IF BLANK THEN
                    bne       IN2
                    ldab      COUNT
                    cmpb      #3                  ; IF COUNT = 3 (LEADING BLANKS)
                    beq       IN1                 ; KEEP GOING
                    bra       IN3                 ; ELSE DONE

IN2                 cmpa      #CR                 ; IF CR THEN
                    beq       IN3                 ; DONE
                    cmpa      #$2C                ; IF COMA THEN
                    beq       IN3                 ; DONE
                    cmpa      #'0'                ; IF < 0 THEN
                    blt       IN5                 ; ERROR
                    cmpa      #'9'                ; IF > 9 THEN
                    bhi       IN5                 ; ERROR
                    dec       COUNT               ; IF THIRD DIGIT THEN
                    beq       IN5                 ; ERROR
                    anda      #$0F                ; STRIP OFF HIGH BITS
                    psha
                    ldaa      ROOMNO              ; MULTIPLY ROOMNO
                    tab                           ; BY 10
                    asla
                    asla
                    asla
                    aba
                    aba
                    pulb
                    aba                           ; ADD NEW DIGIT
                    staa      ROOMNO
                    bra       IN1                 ; GET NEXT CHAR

;**SKIP
;
; ROOMIN CONTINUED
;
IN3                 staa      DELIM               ; SAVE DELIMITER
                    ldaa      ROOMNO
                    beq       IN45
                    ldx       #TRANS              ; LOOK UP
IN31                cmpa      1,X                 ; TRANSLATION
                    beq       IN4
                    inx
                    inx
                    cpx       #TRANS+40           ; IF FOUND
                    bne       IN31                ; DONE
                    bra       IN5                 ; ELSE ERROR

IN4                 ldaa      ,X
IN45                bsr       PULX                ; RESTORE IX
                    rts

IN5                 ldx       #CRLF
                    jsr       STROUT
                    ldx       #TX04               ; WRITE ERROR
                    jsr       STROUT
                    bra       ROOMIN+3            ; TRY AGAIN

;**SKIP
;
;
ROOMOUT             tsta
                    bpl       OUT0
                    jsr       BLKOUT              ; IF BLOCKED TUNNEL THEN
                    jsr       BLKOUT              ; WTRITE SPACES
                    jsr       BLKOUT
                    rts

OUT0                jsr       PSHX                ; SAVE IX
                    jsr       BLKOUT              ; WRITE BLANK
                    ldx       #TRANS              ; IF A = 1
                    deca
                    beq       OUT5                ; GOTO END
OUT1                inx                           ; LOOP THROUGH
                    inx                           ; TABLE
                    deca
                    bne       OUT1
OUT5                ldaa      1,X                 ; TRANSLATE ROOMNO
                    cmpa      #9
                    bhi       OUT7
                    jsr       BLKOUT
OUT7                bsr       DECOUT              ; WRITE ROOM
                    jsr       PULX                ; RESTORE IX
                    rts

;
;
;
DAYSLEFT            jsr       PSHX                ; SAVE IX
                    ldx       #TX16               ; MSG1
                    jsr       STROUT
                    ldaa      DAYS
                    bsr       DECOUT              ; DAYS OUT
                    ldx       #TX17               ; MSG2
                    jsr       STROUT
                    jsr       PULX
                    rts

;
;
;
DECOUT              clr       COUNT
DEC1                inc       COUNT               ; LOAD COUNT
                    suba      #10                 ; WITH TENS DIGIT
                    bpl       DEC1
                    adda      #10
                    dec       COUNT
                    psha                          ; SAVE UNITS DIGIT
                    ldaa      COUNT               ; IF COUNT
                    beq       DEC2                ; NOT = 0
                    oraa      #$30                ; MAKE ASCII
                    jsr       CHAROUT             ; AND PRINT
DEC2                pula                          ; LOAD UNITS
                    oraa      #$30                ; MAKE ASCII
                    jsr       CHAROUT
                    rts

;**SKIP
;
;
RAND                pshb                          ; RANDOM NUMBER GENERATOR - SAVE B
                    ldaa      STUF+1              ; COMPUTE (STUF * 2 * * 9) MOD 2 ** 16
                    clc
                    rola
                    clc
                    rola
                    adda      STUF                ; ADD STUFF TO RESULT
                    ldab      STUF+1
                    clc                           ; MULTIPLY BY 2 ** 2
                    rolb
                    rola
                    clc
                    rolb
                    rola
                    clc
                    addb      STUF+1              ; ADD STUFF TO RESULT
                    adca      STUF
                    clc
                    addb      #$19                ; ADD HEXADECIMAL 3619 TO THE RESULT
                    adda      #$36
                    staa      STUF                ; STORE RESULT IN STUF
                    stab      STUF+1
                    pulb                          ; RESTORE B
                    rts

;       RMB     5
;       EQU     *
;**SKIP
;
;
RANDMOVE            staa      OLDROOM             ; SAVE CURRENT ROOM
                    ldaa      #50                 ; SET LOOP COUNTER
                    staa      COUNT               ; TO 50
                    stx       SAVX                ; SAVE CURRENT IX
RMV1                ldx       SAVX                ; LOOP
                    dec       COUNT               ; PICK FIRST, SECOND,
                    bsr       RAND                ; OR THIRD ROOM
                    cmpa      #85
                    bls       RMV5
                    inx
                    cmpa      #170
                    bls       RMV5
                    inx
RMV5                tst       COUNT               ; IF 50 TRIES,
                    beq       RMV7                ; DONT MOVE
                    ldaa      ,X                  ; IF BLOCKED
                    bmi       RMV1                ; GO BACK TO LOOP
                    bsr       PNTROOM             ; POINT IX TO ROOM
                    tst       3,X                 ; IF ROOM EMPTY,
                    beq       RMV9                ; GO TO END
                    tstb      ELSE                ; IF WUMPUS MOVING
                    bne       RMV1                ; GO BACK TO LOOP
                    bra       RMV9

RMV7                ldaa      OLDROOM
RMV9                ldx       SAVX
                    rts                           ; RETURN

;
;
;
RANDROOM            bsr       RAND                ; GET ROOM
                    anda      #$1F                ; MAKE <= 32
                    cmpa      #20                 ; IF > 20 THEN
                    bhi       RANDROOM            ; DO IT AGAIN
                    tsta                          ; IF = 0 THEN
                    beq       RANDROOM
                    bsr       PNTROOM             ; POINT INDEX
                    rts                           ; RETURN

;**SKIP
;
;
PNTROOM             psha                          ; SAVE A
                    deca                          ; A = 4(A-1) + 1
                    asla
                    asla
                    inca
                    ldx       #ROOMS              ; POINT INDEX
PNT1                deca                          ; LOOP A = 1 TIMES
                    beq       PNT2
                    inx
                    bra       PNT1

PNT2                pula                          ; RESTORE A
                    rts                           ; RETURN

;
;
;
PNTRANS             psha                          ; SAVE A
                    deca                          ; A = 2(A-1) + 1
                    asla
                    inca
                    ldx       #TRANS              ; POINT INDEX
PNT3                deca                          ; LOOP A = 1 TIMES
                    beq       PNT4
                    inx
                    bra       PNT3

PNT4                pula                          ; RESTORE A
                    rts                           ; RETURN

;
;
;
PROMPTIN            jsr       CHARIN              ; GET ANSW
                    psha                          ; PUSH IT
                    cmpa      #CR                 ; IF A = CR THEN
                    beq       PROMPT9             ; GO TO END
PROMPT5             jsr       CHARIN              ; LOOP UNTIL CR
                    cmpa      #CR
                    bne       PROMPT5
PROMPT9             pula                          ; RESTORE ANSWER
                    rts                           ; RETURN

; *SKIP
;
;
PROBLITY            jsr       RAND                ; GET RAND NUMBER
                    anda      #$7F                ; MAKE <= 128
                    cmpa      #100                ; IT > 100 THEN
                    bhi       PROBLITY            ; DO IT AGAIN
                    cmpa      ,X                  ; IF RAND <= PROB
                    bls       PRB1                ; SET A
                    clra                          ; ELSE CLEAR A
                    bra       PRB5

PRB1                ldaa      #1
PRB5                inx                           ; INCREMENT IX
                    rts                           ; RETURN

;
;
;
CONECTED            bsr       PNTROOM             ; ANSWER IS IN B
                    cmpb      ,X                  ; TEXT 3 CONNECTIONS
                    beq       CON5
                    cmpb      1,X
                    beq       CON5
                    cmpb      2,X
                    beq       CON5
                    clrb                          ; B = 0 IF NOT CONNECTED
                    rts

CON5                ldab      #1                  ; ELSE B = 1
                    rts

;
;
;
WON                 ldx       #TX14
                    jsr       STROUT
                    inc       END
                    rts

;
;
;
LOST                ldx       #TX12
                    jsr       STROUT
                    inc       END
                    rts

;**SKIP
;
;
MAIN                lds       #$0FFF              ; SET STACK !!!!!!!!
MAN1                ldx       #TX36               ; ASK FOR INSTRUCTIONS
                    jsr       STROUT
                    bsr       PROMPTIN
                    anda      #$DF                ; CONVERT TO UPPER CASE
                    cmpa      #'Y'
                    beq       TEACH               ; SHOW THEM
                    cmpa      #'N'
                    beq       STARTGAM            ; OR NOT
                    ldx       #WHAT
                    jsr       STROUT
                    bra       MAN1

TEACH               ldx       #TX99
                    jsr       STROUT
STARTGAM            bsr       GENCAVES            ; INIT CAVES
                    jsr       PLAY
MAN2                ldx       #TX35               ; ASK FOR MAP OF CAVE
                    jsr       STROUT
                    bsr       PROMPTIN
                    anda      #$DF                ; CONVERT TO UPPER CASE
                    cmpa      #'Y'
                    beq       DSPLAY              ; SHOW IT
                    cmpa      #'N'
                    beq       NXTGAME             ; OR NOT
                    ldx       #WHAT
                    jsr       STROUT
                    bra       MAN2

DSPLAY              jsr       DISPLAY
NXTGAME             ldx       #TX34               ; ASK FOR ANOTHER GAME
                    jsr       STROUT
                    jsr       PROMPTIN
                    anda      #$DF                ; CONVERT TO UPPER CASE
                    cmpa      #'Y'
                    beq       STARTGAM            ; PLAY IT
                    cmpa      #'N'
                    beq       FINAL               ; OR NOT
                    ldx       #WHAT
                    jsr       STROUT
                    bra       NXTGAME

FINAL               ldx       #TX37
                    jsr       STROUT
                    ldx       #CRLF
                    jsr       STROUT
                    jmp       EXIT                ; !!!!!!!!

;**SKIP
;
;
GENCAVES            bsr       CLEAR               ; CLEAR OLD ITEMS
                    ldaa      #1                  ; SET UP 1 WUMPUS
                    staa      COUNT
                    ldab      #'W'
                    stab      SYMBOL
                    jsr       GENITEM
                    ldab      #'P'                ; SET UP 1 TO 2 PITS
                    stab      SYMBOL
                    jsr       RAND
                    anda      #1
                    inca
                    staa      COUNT
                    bsr       GENITEM
                    ldab      #'B'                ; SET UP 1 TO 4 BAT COLONIES
                    stab      SYMBOL
                    jsr       RAND
                    anda      #3
                    inca
                    staa      COUNT
                    bsr       GENITEM
GEN1                jsr       RANDROOM            ; SET CURRENT ROOM
                    tst       3,X
                    bne       GEN1
                    staa      CURRENT
                    ldx       #TRANS              ; IF TRANSLATION TABLE
                    tst       1,X                 ; IS NOT FILLED IN,
                    bne       GEN6
                    ldab      #20
GEN2                pshb
                    jsr       RANDROOM            ; LOAD 20 ITEMS INTO
GEN3                jsr       PNTRANS             ; TRANSLATION TABLE
                    tst       1,X
                    beq       GEN5
                    adda      #7
                    cmpa      #20
                    bls       GEN3
                    suba      #20
                    bra       GEN3

GEN5                pulb
                    stab      1,X
                    decb
                    bne       GEN2
GEN6                clr       AWAKE               ; PUT WUMPUS TO SLEEP
                    jsr       RANDROOM
                    asra      GEN10               ; TO 20 DAYS
                    adda      #10
                    staa      DAYS
                    ldaa      #5                  ; SET UP 5 SHOTS
                    staa      SHOTS
                    clr       ROCKS               ; AND NO ROCKS
                    clr       END                 ; START NEW GAME
                    rts                           ; RETURN

;**SKIP
;
;
CLEAR               ldx       #ROOMS              ; CLEAR ROOM LOOP
CLR1                ldaa      #3
CLR2                tst       ,X
                    bpl       CLR3
                    com       ,X                  ; CLEAR BLOCKED TUNNELS
CLR3                inx
                    deca
                    bne       CLR2
                    clr       ,X
                    inx
                    cpx       #ROOMS+80
                    bne       CLR1
;
;
;
                    nop                           ; REPLACE THIS WITH RTS (39) TO PREVENT
;                         RESHUFFELING OF ROOM NUMBERS BETWEEN GAMES
;
;
                    ldx       #TRANS              ; CLEAR TRANSLATION TABLE
CLR4                clr       1,X
                    inx
                    inx
                    cpx       #TRANS+40
                    bne       CLR4
                    rts

;
;
;
GENITEM             ldab      COUNT               ; LOOP FOR # OF ITEMS
GEN7                pshb
                    jsr       RANDROOM
                    tst       3,X                 ; FIND EMPTY ROOM
                    bne       GEN7+1
                    ldab      SYMBOL              ; LOAD WITH ITEM
                    stab      3,X
                    cmpb      #'W'                ; IF ITEM IS WUMPUS THEN
                    bne       GEN8
                    staa      WUMPROOM
GEN8                pulb                          ; ELSE DONT
                    decb                          ; CYCLE THROUGH LOOP
                    bne       GEN7
                    rts

;**SKIP
;
;
PLAY                dec       DAYS                ; DECREASE DAYS
                    tst       AWAKE
                    bls       GAME1
                    bsr       MOVE_W              ; MOVE WUMPUS
GAME1               bsr       REPORT              ; REPORT
                    tst       END                 ; IF END
                    bls       GAME3
                    rts                           ; RETURN

GAME3               ldx       #TX03               ; PROMPT FOR SHOOT OR MOVE
                    jsr       STROUT
                    jsr       PROMPTIN
                    anda      #$DF                ; CONVERT TO UPPER CASE
                    cmpa      #'S'
                    beq       GAME4
                    cmpa      #'M'
                    beq       GAME5
                    ldx       #WHAT
                    jsr       STROUT
                    bra       GAME3

GAME4               jsr       SHOOT               ; SHOOT
                    bra       GAME6

GAME5               jsr       MOVE                ; MOVE
GAME6               tst       END
                    bls       GAME8
                    rts

GAME8               ldx       #CRLF               ; SKIP A LINE
                    jsr       STROUT
                    jsr       HAPNINGS            ; RANDOM ITEMS
                    bra       PLAY

;
;
;
MOVE_W              ldx       #PROB3              ; ONE TIME IN FIVE,
                    jsr       PROBLITY            ; DO NOT MOVE WUMPUS
                    tsta
                    bne       MOVE1
                    ldaa      WUMPROOM            ; MOVE TO RANDOM ROOM
                    jsr       PNTROOM
                    ldab      #1
                    jsr       RANDMOVE
                    staa      WUMPROOM
                    clr       3,X                 ; CLEAR CURRENT WUMMPUS INDICATOR
                    jsr       PNTROOM
                    ldab      #'W'
                    staa      3,X                 ; SET NEW WUMPUS INDICATOR
MOVE1               rts

;**SKIP
;
;
REPORT              tst       DAYS                ; IF DAYS = 0 THEN
                    bpl       RPT0
                    ldx       #TX08               ; STARVED TO DEATH
                    jsr       STROUT
                    jsr       LOST
                    rts

RPT0                ldaa      CURRENT
                    jsr       PNTROOM
                    ldab      3,X
                    cmpb      #'B'                ; STEPPED INTO A
                    bne       RPT1                ; BAT CAVE
                    jsr       BATS
                    bra       REPORT              ; START AGAIN

RPT1                cmpb      #'W'                ; BUMBED INTO WUMPUS
                    bne       RPT2
                    jsr       BUMP_W
                    tst       END                 ; IF ASLEEP !!!!!!!!
                    bls       RPT2                ; CONTINUE
                    rts                           ; ELSE END

RPT2                cmpb      #'P'                ; STEPPED INTO A PIT
                    bne       RPT3
                    jsr       PITS
                    rts

;**SKIP
;
;  REPORT CONTINUED
;
RPT3                ldaa      ,X
                    bsr       RPTROOM             ; REPORT SURROUNDING ROOMS
                    ldaa      1,X
                    bsr       RPTROOM
                    ldaa      2,X
                    bsr       RPTROOM
                    ldx       #TX01               ; REPORT CURRENT ROOM
                    jsr       STROUT
                    ldaa      CURRENT
                    jsr       ROOMOUT
                    ldx       #TX02               ; REPORT CONNECTING TUNNELS
                    jsr       STROUT
                    ldaa      CURRENT
                    jsr       PNTROOM
                    jsr       RAND                ; JUMBLE REPORTING
                    anda      #$07                ; OF TUNNELS
                    staa      COUNT
RPT4                ldaa      2,X
                    ldab      1,X
                    stab      2,X
                    ldab      ,X
                    stab      1,X
                    staa      ,X
                    dec       COUNT
                    bne       RPT4
                    ldaa      ,X                  ; REPORT CONECTING
                    jsr       ROOMOUT             ; ROOMS
                    ldaa      1,X
                    jsr       ROOMOUT
                    ldaa      2,X
                    jsr       ROOMOUT
                    jsr       DAYSLEFT            ; REPORT DAYS LEFT
                    rts                           ; RETURN

;**SKIP
;
;
RPTROOM             jsr       PSHX                ; SAVE IX
                    tsta
                    bmi       RRM9
                    jsr       PNTROOM
                    ldaa      3,X
                    cmpa      #'P'                ; REPORT
                    bne       RRM1
                    ldx       #TX06               ; I FEEL A DRAFT
                    jsr       STROUT
RRM1                cmpa      #'B'
                    bne       RRM2
                    ldx       #TX05               ; I HEAR WINGS
                    jsr       STROUT
RRM2                cmpa      #'W'
                    bne       RRM9
                    ldx       #TX07               ; I SMELL WUMPUS
                    jsr       STROUT
RRM9                jsr       PULX                ; RESTORE IX
                    rts                           ; RETURN

;**SKIP
;
;
BATS                jsr       PSHX                ; SAVE IX
                    ldx       #TX20               ; WHOOPS
                    jsr       STROUT
                    jsr       RANDROOM            ; PICK A ROOM
                    staa      CURRENT             ; MOVE TO IT
                    ldab      DAYS
                    tba
                    asrb
                    asrb
                    sba
                    staa      DAYS
                    jsr       PULX                ; RESTORE IX
                    rts

;**SKIP
;
;
PITS                ldx       #TX09               ; REPORT PIT
                    jsr       STROUT
                    jsr       LOST
                    rts

;
;
;
BUMP_W              jsr       PSHX
                    tst       AWAKE               ; IF NOT AWAKE
                    bhi       BMP5
                    ldx       #CRLF
                    jsr       STROUT
                    ldx       #TX28               ; WATCH IT
                    jsr       STROUT
                    ldx       #TX33               ; PRINT WARNING
                    jsr       STROUT
                    ldx       #CRLF
                    jsr       STROUT
                    ldx       #TX29               ; IS AWAKE
                    jsr       STROUT
                    inc       AWAKE               ; WAKE HIM UP
                    jsr       PULX
                    rts

BMP5                ldx       #TX11               ; ELSE PRINP CHOMP
                    jsr       STROUT
                    jsr       LOST
                    jsr       PULX
                    rts

;**SKIP
;
;
SHOOT               tst       SHOTS               ; IF NO SHOTS
                    bne       S10
                    jsr       THROW               ; CALL THROW
                    rts

S10                 clr       WILD                ; CLEAR INDICATORS
                    clr       STOP
                    dec       SHOTS
                    ldx       #CRLF
                    jsr       STROUT
                    ldx       #TX04               ; WHERE TO?
                    jsr       STROUT
;       LDX     #CRLF
;       JSR     STROUT
                    ldx       #ARRAY
                    stx       ARRAYNO
                    ldaa      CURRENT
                    staa      TEST
                    ldab      #1
S11                 stab      ROOMTLY
                    jsr       ROOMIN              ; LOAD UP TO 3 ROOMS
                    tsta
                    beq       S13                 ; END ROOMS WHEN NO ROOM INPUT
                    staa      ,X
                    inx
                    inc       ROOMTLY             ; !!!!!!!!
                    ldaa      DELIM               ; IF DELIMITER IS A CARRIAGE RETURN,
                    cmpa      #CR                 ; GO TO END OF ROOM LIST
                    beq       S13
                    ldab      ROOMTLY
                    cmpb      #4
                    bne       S11
S12                 jsr       ROOMIN              ; IGNORE MORE ROOMS
                    tsta
                    beq       S13
                    ldaa      DELIM               ; IF DELIMITER IS CARRIAGE RETURN,
                    cmpa      #CR                 ; GO TO END OF ROOM LIST
                    bne       S12
S13                 tst       AWAKE               ; IF WUMPUS IS ASLEEP THEN
                    bhi       S20
                    ldx       #CRLF               ; REPORT HIM AWAKE
                    jsr       STROUT
                    ldx       #TX28
                    jsr       STROUT
                    ldx       #TX29
                    jsr       STROUT
                    inc       AWAKE               ; AND WAKE HIM UP
;**SKIP
;
;  SHOOT CONTINUED
;
S20                 dec       ROOMTLY             ; LOOP FOR NUMBER OF ROOMS
                    bne       S21
                    rts

S21                 tst       WILD                ; IF SHOT IS ALREADY WILD THEN
                    bhi       S22                 ; GO TO RANDOM SHOT
                    ldx       ARRAYNO             ; GET NEXT ROOM
                    inc       ARRAYNO+1           ;**NOTE:ARRAY MUST NOT CROSS PAGE BOUNDRY !!!
                    ldab      ,X
                    pshb
                    ldaa      TEST                ; IF NEXT ROOM IS CONNECTED TO CURRENT ROOM THEN
                    jsr       CONECTED
                    tstb
                    bhi       S25                 ; CONTINUE
                    ldx       #TX30               ; ELSE PRIND WILD SHOT
                    jsr       STROUT              ; FOR FIRST ROOM WILD
                    ldx       #TX28               ; (WATCH IT)
                    jsr       STROUT
                    inc       WILD
                    ins                           ; IGNORE PUSHED ROOM
S22                 ldaa      TEST                ; MAKE RANDOM MOVE
                    jsr       RANDMOVE
                    psha
S25                 pula
                    staa      TEST                ; LOOK AT ROOM SHOT INTO
                    jsr       PNTROOM
                    ldaa      3,X
                    cmpa      #'W'
                    bne       S31
                    jsr       WON                 ; IF SHOT WUMPUS THEN
                    inc       END                 ; GAME WON
                    rts

S31                 cmpa      #'B'
                    bne       S32
                    jsr       STBAT               ; IF SHOT BATS
                    tst       STOP                ; THEN CALL STBAT
                    bls       S32
                    rts

S32                 cmpa      #'P'
                    bne       S33
                    jsr       STPIT               ; IF SHOT PIT
                    tst       STOP                ; THEN CALL STPIT
                    bls       S33
                    rts

S33                 ldaa      TEST
                    cmpa      CURRENT             ; IF SHOT SELF
                    bne       S20                 ; THEN CALL STSELF
                    jsr       STSELF
                    rts

;**SKIP
;
;
THROW               tst       ROCKS
                    bne       THR1
                    ldx       #TX38               ; NOTHING TO SHOW
                    jsr       STROUT
                    rts

THR1                dec       ROCKS               ; USE ROCK
THR0                ldx       #TX04               ; WHERE?
                    jsr       STROUT
                    jsr       ROOMIN
                    ldx       #CRLF
                    jsr       STROUT
                    tsta
                    beq       THR0                ; ENTER ONE ROOM ONLY
                    ldab      CURRENT             ; SEE IF CONNECTED ROOM
                    jsr       CONECTED
                    tstb
                    bhi       THR2                ; IF SO CONTINUE
                    ldx       #TX22               ; ELSE PRINT MISSED
                    jsr       STROUT
                    ldx       #TX39               ; NOT CONNECTED
                    jsr       STROUT
                    rts

THR2                jsr       PNTROOM
                    ldab      3,X
                    cmpb      #'W'                ; IF HIT WUMPUS
                    beq       THR5                ; GO TO HIT WUMPUS (5)
                    cmpb      #'B'                ; IF HIT BATS
                    bne       THR3
                    jsr       MIGRATN             ; CAUSE MIGRATION
THR3                ldx       #TX22               ; MISSED
                    jsr       STROUT
                    rts

THR5                ldx       #PROB8              ; HIT WUMPUS WITH A ROCK
                    jsr       PROBLITY
                    tsta
                    bls       THR6
                    ldx       #TX13               ; BONK! YOU WIN
                    jsr       STROUT
                    jsr       WON
                    ldx       #TX15               ; WITH A ROCK
                    jsr       STROUT
                    rts

THR6                ldx       #TX21               ; THUD TRY AGAIN
                    jsr       STROUT
                    rts

;**SKIP*
;
;
STBAT               ldx       #PROB1
                    jsr       PROBLITY            ; IF PROBABLE TO
                    tsta                          ; SHOT BAT, DO IT
                    bls       STB9
                    inc       STOP
                    ldx       #TX31               ; SHOT BAT
                    jsr       STROUT
STB9                rts

;
;
;
STPIT               ldx       #PROB2
                    jsr       PROBLITY            ; IF PROBABLE TO
                    tsta                          ; DO IT
                    bls       STP9
                    inc       STOP
                    ldx       #TX32               ; LOST ARROW IN PIT
                    jsr       STROUT
STP9                rts

;
;
;
STSELF              ldx       #PROB3
                    jsr       PROBLITY            ; IF PROBABLE TO INJURE ONLY
                    tsta                          ; DO IT
                    bls       STS7
                    ldx       #TX19               ; SHOT LEG
                    jsr       STROUT
                    ldaa      DAYS
                    cmpa      #5                  ; IF LESS THAN 5 DAYS LEFT
                    bhi       STS3
                    inc       DAYS                ; REDUCE BY HALF
                    lsr       DAYS
                    bra       STS5

STS3                ldaa      #5                  ; ELSE REDUCE TO 5
                    staa      DAYS
STS5                jsr       DAYSLEFT            ; REPORT DAYS LEFT
                    rts

STS7                ldx       #TX10               ; OTHERWISE SHOT SELF
                    jsr       STROUT
                    jsr       LOST
                    rts

;**SKIP
;
;
MOVE                ldx       #CRLF
                    jsr       STROUT
                    ldx       #TX04               ; WHERE TO
                    jsr       STROUT
                    jsr       ROOMIN
                    ldx       #CRLF
                    jsr       STROUT
                    tsta
                    beq       MOVE
                    ldab      CURRENT             ; IF NOT CONNECTED
                    jsr       CONECTED
                    tstb
                    bls       MOVE                ; TRY AGAIN
                    staa      CURRENT             ; SET NEW CURRENT ROOM
                    rts

;
;
;
DISPLAY             ldx       #TX40               ; PRINT HEADING
                    jsr       STROUT
                    ldaa      #1                  ; LOAD COUNTER
                    ldx       #ROOMS
DSP1                jsr       PSHX
                    ldx       #CRLF               ; AND GO TO NEW LINE
                    jsr       STROUT
                    jsr       PULX
                    psha
                    jsr       ROOMOUT             ; PRINT ROOM
                    jsr       BLKOUT              ; AND BLANK
                    jsr       BLKOUT              ; CHARACTERS
                    jsr       BLKOUT
                    jsr       BLKOUT
                    ldaa      #3
                    staa      ROOMTLY
DSP2                ldaa      ,X                  ; PRINT 3 CONNECTIONS
                    jsr       ROOMOUT
                    jsr       BLKOUT              ; WITH BLANKS AS SEPARATOR
                    inx
                    dec       ROOMTLY
                    bne       DSP2
                    jsr       BLKOUT              ; AND BLANK
                    jsr       BLKOUT
                    ldaa      ,X
                    jsr       CHAROUT             ; PRINT ROOM CONTENTS
                    inx
                    pula
                    inca                          ; LOOP THROUGH
                    cmpa      #21                 ; 20 ROOMS
                    bne       DSP1
                    rts

;**SKIP
;
;
HAPNINGS            ldx       #PROB4              ; POINT TO PROBABILITIES
                    jsr       PROBLITY
                    tsta                          ; TEST FOR BAT MIGRATION
                    bls       HAP1
                    bsr       MIGRATN
HAP1                jsr       PROBLITY
                    tsta                          ; TEST FOR FIND A ROCK
                    bls       HAP3
                    bsr       ROCKX
HAP3                jsr       PROBLITY
                    tsta                          ; TEST FOR RATS EATING FOOD
                    bls       HAP5
                    bsr       RATS
HAP5                jsr       PROBLITY
                    tsta                          ; TEST FOR ROCK SLIDE
                    bls       HAP7
                    bsr       SLIDE
HAP7                jsr       PROBLITY
                    tsta                          ; TEST FOR EXTRA SUPPLIES
                    bls       HAP9
                    jsr       EXTRA
HAP9                rts                           ; RETURN

;**SKIP
;
;
MIGRATN             jsr       PSHX                ; SAVE IX
                    jsr       RAND
                    anda      #$03                ; 1 TO 4
                    inca
                    tab
                    ldaa      DAYS
                    sba
                    staa      DAYS
                    ldx       #ROOMS
MIG1                ldaa      3,X                 ; FIND SOME BATS
                    cmpa      #'B'
                    beq       MIG2
                    inx
                    inx
                    inx
                    inx
                    bra       MIG1

MIG2                jsr       PSHX                ; SAVE BAT LOCATION
                    ldaa      #1
                    staa      COUNT
                    jsr       GENITEM             ; LOAD NEW BATS RANDOMLY
                    jsr       PULX                ; RESTORE OLD BAT LOCATION
                    clr       3,X                 ; CLEAR OLD BATS
                    ldx       #TX18               ; REPORT BATS
                    bsr       STROUT
                    jsr       PULX                ; RESTORE IX
                    rts

;
;
;
ROCKX               tst       SHOTS               ; IF ARROWS LEFT
                    beq       ROK1
                    rts                           ; RETURN

ROK1                jsr       PSHX                ; SAVE IX
                    inc       ROCKS               ; ADD ROCKS TO PILE
                    ldx       #TX23               ; REPORT FIND
                    bsr       STROUT
                    ldx       #TX24
                    bsr       STROUT
                    jsr       PULX                ; RESTORE IX
                    rts

;**SKIP
;
;
RATS                jsr       PSHX                ; SAVE IX
                    inc       DAYS
                    asr       DAYS                ; REDUCE DAYS BY HALF
                    ldx       #TX26               ; REPORT RATS
                    bsr       STROUT
                    jsr       PULX                ; RESTORE IX
                    rts

;
;
;
SLIDE               jsr       PSHX                ; SAVE IX
SLD1                jsr       RANDROOM            ; GET A TUNNEL
                    staa      OLDROOM
                    ldaa      1,X
                    bmi       SLD1                ; IGNORE IF ALREADY CLOSED
                    com       1,X                 ; CLOSE HALF TUNNEL
                    jsr       PNTROOM             ; POINT TO OTHER HALF
                    ldaa      OLDROOM
                    cmpa      ,X
                    beq       SLD5                ; FIND RIGHT TUNNEL BACK
                    inx
                    cmpa      ,X
                    beq       SLD5
                    inx
SLD5                com       ,X                  ; CLOSE OTHER HALF
                    ldx       #TX27               ; REPORT SLIDE
                    bsr       STROUT
                    jsr       PULX                ; RESTORE IX
                    rts

;**SKIP
;
;
EXTRA               jsr       PSHX                ; SAVE IX
                    jsr       RAND                ; INCREASE DAYS BY
                    anda      #$03                ; 1 TO 4
                    inca
                    tab
                    ldaa      DAYS
                    aba
                    staa      DAYS
                    ldx       #TX23               ; REPORT FIND
                    bsr       STROUT
                    ldx       #TX25
                    bsr       STROUT
                    jsr       PULX                ; RESTORE IX
                    rts

;
;
; EXTERNAL REFERENCES
;
CHARIN              jmp       $E1AC               ; MONITOR: INCHAR
CHAROUT             jmp       $E1D1               ; MONITOR: OUTCHAR

STROUT              psha
                    jsr       $E07E               ; MOINTOR: PDATA
                    pula
                    rts

BLKOUT              psha
                    ldaa      #$20
                    bsr       CHAROUT
                    pula
                    rts

EXIT                jmp       $E0D0               ; MONITOR: RETURN
