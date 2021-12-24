;*****************
; 18 WITH A DIE *
; R.YOST 790718 *
; BYTE V05 N01  *
;*****************

CONTRL              equ       $E0D0               ; RETURN TO MONITOR
INEEE               equ       $E1AC               ; INPUT CHAR FROM CONSOLE AND ECHO
PDATA               equ       $E07E               ; PRINT TEXT STRING @ X ENDED BY $04
OUTHR               equ       $E06B               ; PRINT RIGHT HEX CHAR @ X
OUT2HS              equ       $E0CA               ; PRINT 2 HEX CHARS @ X

; VARIABLES AND TABLES
                    org       $20
MSKTBL              rmb       7                   ; LEGAL RESPONSES MASKS
WINTBL              rmb       18                  ; TABLE OF WINNING PLAY BIT PATTERNS
                    rmb       7                   ; SPACE FOR END GAME MARKERS
MPTPAT              rmb       1                   ; M. TRIAL PLAY PATTERN
SCOREH              rmb       1                   ; H. AND M. BCD SCORES
SCOREM              rmb       1
TOTAL               rmb       1                   ; BINARY TOTAL OF ALL PLAYS
TOTBCD              rmb       1                   ; DITTO, BCD
HPLAY               rmb       1                   ; VALID, LEAGAL, H. PLAY
LSTPLA              rmb       1                   ; OPPONENT'S PREV. PLAY
HTURN               rmb       1                   ; H. TIRN FLAG
SCNFLG              rmb       1                   ; SCAN WITH TABLE FLAG. 1 MEANS
;                               BEFORE M'S PLAY
;                               0 MEANS AFTER M'S PLAY.
ENDGAM              rmb       1                   ; END GAME FLAG. SET IF TOTAL > 17
LTSTIN              rmb       2                   ; DUMMY INPUT VARIABLE FOR LEGAL TEST
;                        2 BYTES ACCOMODATE X REG.
WHRF                rmb       6                   ; TABLE STORES WINNING HUMAN RESPONSES
;                        TO TRIAL M PLAYS.
MPLAY               rmb       1                   ; TRIAL M PLAY.
MPLAW               rmb       1                   ; WINNING PLAY DETECTED IN REVISING
;                        WIN TABLE.
SSTO                rmb       2                   ; TEMP. STK. PNTR STORAGE
WTBLRP              rmb       2                   ; WINTABLE ROW POINTER
RANUM               rmb       2                   ; RANDOM NUMBER SHIFT REGISTERS.

; MAIN PROGRAM
                    org       $100
;       LDS     #$A047          PRESERVE STRT ADDR @ $A048,9.
                    jsr       INITLZ
                    ldx       #MES1               ; PRINT INSTRUCTIONS AND
                    jsr       TXTOUT              ; ASK "READY TO PLAY".
                    jsr       INEEE               ; GET RESPONSE
                    anda      #$DF                ; TO UPPER CASE
                    cmpa      #'Y'                ; IF NOT 'Y' THEN
                    beq       M2
M1                  ldx       #MES5               ; PRINT "THANKS, GOOD BYE",
                    bsr       TXTOUT
                    jmp       CONTRL              ; AND EXIT TO MIKBUG.

M2                  clra                          ; ELSE, INITIALIZE VARS. FOR
                    staa      TOTAL               ; NEW GAME.
                    staa      TOTBCD
                    staa      LSTPLA
                    ldx       #MES2               ; PRINT NEW GAME HEADER.
                    bsr       TXTOUT
                    ldx       #SCOREH             ; PRINT SCORES.
                    jsr       OUT2HS
                    ldx       #SCOREM
                    jsr       OUT2HS
                    ldx       #MES3               ; PRINT "YOUR 1ST PLAY?"
                    bsr       TXTOUT
M3                  jsr       GTVLHP              ; GET VALID LEGAL H. PLAY.
                    ldaa      HPLAY               ; PUT I HPLAY AND LAST PLAY.
                    staa      LSTPLA
                    ldaa      #1
                    staa      HTURN               ; SET H. TURN FLAG,
                    staa      SCNFLG              ; AND SCAN FLAG, TO DENOTE WIN TABLE
M4                  jsr       AVAZTO              ; REVISION BEFORE M. PLAY. ADD PLAY
;                                TO TOTALS. SET END GAME FLG. IF
;                                TOTAL > 17, AND CALC. SCORES.
                    tst       ENDGAM
                    beq       M5
                    ldx       #SCOREH             ; IF GAME OVER, PRINT SCORES.
                    jsr       OUT2HS
                    ldx       #SCOREM
                    jsr       OUT2HS
                    ldx       #MES4               ; PRINT "NEW GAME?"
                    bsr       TXTOUT
                    jsr       INEEE
                    anda      #$DF
                    cmpa      #'N'                ; IF RESPONSE = 'N', PRINT THANKS, ETC,
                    beq       M1                  ; AND EXIT.
                    bra       M2                  ; ELSE, SET UP NEW GAME.

M5                  jsr       RVSTBW              ; IF GAME NOT OVER, LOOK AHEAD FOR
;                                H RESPONSES TO M TRIAL PLAYS
;                                AND REVERSE WIN TABLE.
                    tst       SCNFLG              ; IF SCAN FLAG CLEAR, THEN
                    beq       M6                  ; GET NEXT H. PLAY
                    jsr       SELMPL              ; ELSE, SELECT M. PLAY.
                    ldaa      MPLAY
                    staa      LSTPLA
                    clra                          ; CLEAR SCAN FLAG TO REVISE WIN TABLE BY
;                                LOOKING AHEAD AFTER M'S PLAY.
                    staa      SCNFLG
                    staa      HTURN               ; CLEAR H. TURN FLAG.
                    ldx       #MES6               ; ANNOUNSE M'S PLAY,
                    bsr       TXTOUT
                    ldaa      MPLAY
                    jsr       OUTHR
                    bra       M4                  ; AND PROCESS IT.

M6                  ldx       #MES7               ; IF SCAN FLAG WAS RESET, REQUEST
                    bsr       TXTOUT              ; NEXT H. PLAY.
                    bra       M3                  ; AND GET IT.

TXTOUT              jmp       PDATA               ; TARGET FOR MIKBUG STRING PRINT

; SUBROUTINE INITIALIZE - INITLZ
;       ORG     $5D
                    org       *
INITLZ              ldx       #MSKTBL             ; INITIALIZE MASK TABLE WHIT
                    ldaa      #$3F                ; LEGAL RESPONSE PATTERNS
                    staa      0,X                 ; IN 6 LEAST SIG. BITS
                    ldaa      #$1E
                    staa      1,X
                    staa      6,X
                    ldaa      #$2D
                    staa      2,X
                    staa      5,X
                    ldaa      #$33
                    staa      3,X
                    staa      4,X
                    clra                          ; CLEAR WIN TABLE BEFORE
                    ldx       #1                  ; FIRST GAME.
I1                  staa      WINTBL-1,X
                    inx
                    cpx       #$13
                    bne       I1
                    ldaa      #3                  ; STORE DUMMY H WIN PLAY
I2                  staa      WINTBL-1,X          ; BITS BEYOND END OF WINTABLE
                    inx                           ; SO MACHINE CAN SEE END OF
                    cpx       #$19                ; GAME COMING.
                    bne       I2
                    staa      RANUM+1             ; INITIALIZE RANDOM NUMBER GENERATOR
                    clra                          ; SEED.
                    staa      LTSTIN              ; CLEAR HIGH BYTE OF L. TEST, ALSO
                    staa      SCOREH              ; DIMMY VAR. AND SCORES.
                    staa      SCOREM
                    rts

; SUBROUTINE GTVLHP - GET VALID LEGAL H. PLAY.
;       ORG     $1A0
GTVLHP              jsr       INEEE               ; GET H. KEYBOARD INPUT.
                    suba      #$30                ; CONVERT TO HEX
                    ble       G1                  ; OF LESS THAN 1,
                    cmpa      #6
                    ble       G2                  ; OR GREATER THAN 6.
G1                  ldx       #MES8               ; PRINT "INVALID"
                    bsr       TXTOUT
                    bra       GTVLHP              ; AND TRY AGAIN.

G2                  staa      LTSTIN+1            ; CHECK H. PLAY FOR LEGALITY
                    bsr       LGLTST              ; ZERO (Z) BIT OF C REG=0 IF
                    bne       G3                  ; PLAY IS LEGAL.
                    ldx       #MES9               ; IF NOT, PRINT "ILLEGAL"
                    bsr       TXTOUT
                    bra       GTVLHP              ; AND TRY AGAIN.

G3                  ldaa      LTSTIN+1            ; STORE VALID LEGAL H. PLAY.
                    staa      HPLAY
                    rts

; LEGAL TEST S.R., LGLTST
;       ORG     $1C8
LGLTST              ldaa      LTSTIN+1            ; 0 AND 7 INPUTS
                    beq       L1                  ; FROM RAND. NO. GEN. ARE TREATED AS
                    cmpa      #7                  ; ILLEGAL.
                    beq       L1
                    cmpa      LSTPLA              ; IF TEST PLAY = LAST PLAY,
                    beq       L1
                    adda      LSTPLA              ; OR IF SUM = 7, THEN
                    cmpa      #7                  ; TEST PLAY IS ILLEGAL.
L1                  rts                           ; 'Z' BIT OF COND'N (C) REG. RETURNS

;                                RETURN OF TEST.

; S.R. ADVANCE AND ANALYZE TOTAL - AVAZTO
;       ORG     $1E0
AVAZTO              clr       ENDGAM              ; INILZ END GAME FLAG
                    ldx       #MESA               ; PRINT "TOTAL IS "
                    bsr       TXTOUT
                    ldaa      LSTPLA              ; GET LAST PLAY
                    ldab      HTURN               ; AND H TURN FLAG
;                                IN 'A' AND 'B' REGS.
                    adda      TOTBCD              ; ADD LAST PLAY TO BCD TOTAL.
                    daa
                    staa      TOTBCD
                    ldx       #TOTBCD             ; SET 'X' AS PNTR FOR MKBG S.R.
                    jsr       OUT2HS              ; PRINT TOTAL.
                    ldaa      LSTPLA              ; NOW, ADD LAST PLAY TO
                    adda      TOTAL               ; BINARY TOTAL.
                    staa      TOTAL
                    suba      #18                 ; IF TOTAL < 18, THEN RETURN.
                    blt       A2
                    inc       ENDGAM              ; ELSE, SET END GAME FLAG.
                    tsta                          ; IF TOTAL NOT IS,
                    beq       A4
                    tstb      AND                 ; IF H'S TURN, THEN
                    beq       A3
A1                  ldx       #MESB               ; PRINT "I WIN",
                    jsr       TXTOUT
                    ldaa      #1                  ; AND INCREMENT M'S SCORE IN BCD.
                    adda      SCOREM
                    daa
                    staa      SCOREM
A2                  rts

A3                  ldx       #MESC               ; IF TOTAL IS 18, AND IF M'S TURN
                    jsr       TXTOUT              ; PRINT "YOU WIN"
                    ldaa      #1                  ; AND INCREMENT H'S SCORE IN BCD.
                    adda      SCOREH
                    daa
                    staa      SCOREH
                    rts

A4                  tstb      ELSE,               ; IF TOTAL IS 18
;                                AND IF M'S TURN, THEN
                    beq       A1                  ; AWARD WIN TO MACHINE.
                    bra       A3                  ; ELSE, IF H'S TURN, AWARD

;                                 WIN TO HUMAN

; SUBROUTINE REVERSE WINTABLE - RVSTBW
;       ORG     $230
RVSTBW              clra
                    staa      MPLAW               ; RESET WINNING PLAY REG. TO 'NONE' STATE.
                    staa      WTBLRP              ; CLEAR W. TABLE PTR. HIGH
;                                BYTE FOR PAGE 0 USE.
                    sts       SSTO                ; SAVE STACK POINTER.
                    ldx       #1                  ; LOAD TABLE POINTER WITH TRIAL
;                                PLAY OF 1.
                    ldaa      #WINTBL-1           ; INLZ W TABLE PNTR LOW BYTE TO
                    adda      TOTAL               ; CORRESPOND TO CURRENT TOTAL.
                    staa      WTBLRP+1
                    lds       WTBLRP              ; SET STK POINTER TO NEXT ROW OF W. TABLE
R1                  pula                          ; GET W. TABLE ROW (BYTE) IN 'A'.
;                               ( THIS IS THE ROW CORRESPONTING TO
;                               THE CURRENT TOTAL PLUS THE TRIAL
;                               MACHINES PLAY.)
                    anda      MSKTBL,X            ; MASK WITH THE LEGAL RESPONSES
;                                TO M'S TRIAL PLAY.
                    staa      WHRF-1,X            ; STORE RESULT IN TBL. OF WINNING
;                                H. RESPONSES TO M TRIAL PLAYS.
                    inx                           ; GET NEXT TRIAL M PLAY.
                    cpx       #7                  ; IF < 7, REPEAT SCAN FOR LEGAL WINNING
                    bne       R1                  ; H RESPONSES.

; NOW, ADD WINNING M PLAYS FOUND ABOVE TO THE WIN TABLE
; IN THE ROW CORRESPONDING TO THE CURRENT TOTAL. LIKE-
; WIZE, DELETE ANY PREVIOUSLY INCORRECTLY DEFINED WIN-
; ING PLAYS FROM THE SAME ROW.

                    ldaa      #$20                ; SET 6TH BIT OF TRIAL PLAY PATTERN.
                    staa      MPTPAT
                    ldx       #6                  ; SELECT M TRIAL PLAY OF 6.
;                               (BY COUNTING DOWN, GET SMALLEST
;                               WINNING PLAY IN MPLAW.)
R2                  lds       WTBLRP              ; SET STK PTR TO CURRENT
                    des                           ; TOTAL ROE (BYTE) OF W. TABLE.
                    tst       WHRF-1,X            ; DID PREVIOUS SCAN DETECT ANY WINNING
;                                LEGAL H RESPONSES TO TRIAL M PLAY?
                    bne       R3                  ; IF NOT, TRIAL M PLAY IS A WINNING PLAY,
                    pula                          ; SO 'OR' THE TRIAL M PLAY BIT PATTERN INTO THE
                    oraa      MPTPAT              ; CURRENT TOTAL ROW OF THE W. TABLE.
                    psha
                    stx       LTSTIN              ; TEST TRIAL PLAY FOR LEGALITY,
                    lds       SSTO                ; AFTER RESTORING STACK POINTER.
                    jsr       LGLTST
                    beq       R4                  ; IF LEAGAL, STORE TRIAL PLAY IN
                    ldaa      LTSTIN+1            ; MPLAW, OVER ANY PREVIOUS LARGER PLAYS.
                    staa      MPLAW
                    bra       R4                  ; SELECT NEXT TRIAL PLAY.

;
R3                  com       MPTPAT              ; IF LEGAL WINNING H RESPONSES TRO TRIAL
                    pula                          ; PLAY WERE DETECTED IN PREVIOUS SCAN,
                    anda      MPTPAT              ; CLEAR TEH BIT (IN CURRENT W. TABLE
                    psha                          ; ROW) CORRESPONDING TO M TRIAL PLAY.
                    com       MPTPAT
R4                  dex                           ; SELECT NEXT (LOWER) M. TRIAL PLAY
                    lsr       MPTPAT              ; MOVE TRIAL PLAY PATTERN ONE BIT TO
;                                CORRESPOND TO NEXT M. TRIAL PLAY.
                    bne       R2                  ; IF PATTERN NOT NULL, REPEAT
;                                W. TABLE REVISION FOR NEXT M. TRIAL PLAY.
                    lds       SSTO                ; RESTORE STACK POINTER.
                    rts

; SUBROUTINE SELECT M. PLAY - SELMPL
;       ORG     $290
SELMPL              bsr       GNRNPL              ; GET RANDOM NUMBER 0 THRU 7,
;                                FOR GOOF DECISION.
                    beq       SP1                 ; IF = 0, GOOF. (WITH SP1, THIS IS
;                                TURNED OFF. USE SP2 HERE TO TURN ON.)
                    !...
SP1                 ldaa      MPLAW               ; IF NOT GOOFING, GET WINNING M PLAY.
                    beq       SP2                 ; IF THERE WAS NONE, GET RANDOM PLAY
                    bra       SP3                 ; AND STORE IN M PLAY REG.

SP2                 bsr       GNRNPL              ; GET RANDOM PLAY, 0 THRU 7
                    staa      LTSTIN+1            ; TEST SELECTED PLAY FOR GELALITY.
                    jsr       LGLTST
                    beq       SP2                 ; IF ILLEGAL, TRY ANOTHER.
                    ldaa      LTSTIN+1            ; ELSE, TRANSFER THE SELECTED PLAY
SP3                 staa      MPLAY               ; TO THE M PLAY REG.
                    rts

; SUBROUTINE GENERATE RANDOM PLAY - GNRNPL
;       ORG     $2B0
GNRNPL              ldx       #3                  ; COUNT 3 SHIFTS, SO 3 'NEW' BITS WILL
;                                BE GENERATED.
GR1                 ldab      #9                  ; MASK BIT PATTERN TO IMPLEMENT
;                                BIT-3 EXOR BIT-0 = 1?
                    andb      RANUM+1             ; APPLY MASK TO LOW BYTE OF R.N.
                    cmpb      #1                  ; TEST TRUE, SO SET CARRY.
                    beq       GR2
                    cmpb      #8                  ; TEST FALSE, SO CLEAR CARRY.
                    clc
                    bne       GR3
GR2                 sec
GR3                 ror       RANUM               ; ROTATE CARRY INTO MSB OF 2 BYTE R.N.
                    ror       RANUM+1
                    dex                           ; DECREMENT SHIFT COUNTER.
                    bne       GR1
                    ldaa      RANUM+1             ; STORE 3 LSB'S OF R.N. IN 'A', AS
                    anda      #7                  ; RANDOM NUMBER 0 THRU 7.
                    rts

; STRING CONSTANTS
CRLF                equ       $D0A                ; CARRIAGE RETURN, LINE FEED
ENDST               equ       4                   ; STRING END SYMBOL FOR MIKBUG PDATA1
LNFD                equ       $A                  ; LINE FEED
LNFDS               equ       $A0A                ; TWO LINE FEEDS

; INSTUCTIONS - MES1
;       ORG     $2D0
MES1                fcb       $1B,$5B,$32,$4A     ; CLEAR SCREEN
;       FDB     CHMEOF
;       FDB     0
;       FCB     0
                    fcc       "HI! LET'S PLAY '18 WITH A DIE'!"
                    fdb       CRLF
                    fcc       "YOU START. CHOOSE ANY NUMBER,"
                    fdb       CRLF
                    fcc       "1 THRU 6. I DO LIKEWISE, BUT WE"
                    fdb       CRLF
                    fcc       "CAN'T PLAY THE NUMBER THE OTHER"
                    fdb       CRLF
                    fcc       "JUST PLAYED, NOR ITS DIFFERENCE"
                    fdb       CRLF
                    fcc       "FROM 7. TRY TO MAKE THE TOTAL"
                    fdb       CRLF
                    fcc       "OF ALL YOUR PLAYS HIT 18 ON"
                    fdb       CRLF
                    fcc       "YOUR PLAY."
                    fdb       CRLF
                    fcb       LNFD
                    fcc       "READY TO START? (TYPE Y OR N.)"
                    fcb       ENDST

; NEW GAME HEADER - MES2
;       ORG     $3F0
MES2                fcb       $1B,$5B,$32,$4A
                    fcc       "      EIGHTEEN WITH A DIE"
                    fdb       CRLF
                    fcb       LNFD
                    fcc       "                    GAMES WON"
                    fdb       CRLF
                    fcc       "                      YOU ME"
                    fdb       CRLF
                    fcc       "TOTAL IS 00            "
                    fcb       ENDST

; YOUE FIRST PLAY? MES3
;       ORG     $465
MES3                fdb       CRLF
                    fcb       LNFD
                    fcc       "        YOUR FIRST PLAY? "
                    fcb       ENDST

; ILLEGAL PLAY - MES9
;       ORG     $482
MES9                fdb       CRLF
                    fcc       "ILLEGAL PLAY! TRY ANOTHER."
                    fcb       ENDST

; INVALID PLAY - MED 8
;       ORG     $4A2
MES8                fdb       CRLF
                    fcc       "INVALID PLAY! PLAY 1 THRU 6."
                    fcb       ENDST

; TOTAL IS - MESA
;       ORG     $4CE
MESA                fcb       $1B,$5B,$48         ; CURSOR HOME
                    fdb       LNFDS
                    fdb       LNFDS
                    fcc       "TOTAL IS "
                    fcb       ENDST

; YOU WIN! - MESC
;       ORG     $4DD
MESC                fcc       "  YOU WIN! "
                    fcb       ENDST

; I WIN! - MESB
;       ORG     $4EA
MESB                fcc       "  I WIN!   "
                    fcb       ENDST

; NEW GAME? - MES4
;       ORG     $4F6
MES4                fcb       LNFD
                    fdb       CRLF
                    fcb       $1B,$5B,$4A         ; CLEAR TO END OF SCREEN
                    fcc       "NEW GAME? ('Y' OR 'N')"
                    fcb       ENDST

; THANKS, ETC. - MES5
;       ORG     $512
MES5                fdb       CRLF
                    fcc       "THANKS FOR PLAYING. HOPE YOU"
                    fdb       CRLF
                    fcc       "ENJOYED IT AS MUCH AS I."
                    fdb       CRLF
                    fcc       "SEE YOU LATER!"
                    fcb       ENDST

; MY PLAY IS - MES6
;       ORG     $55A
MES6                fcb       LNFD
                    fdb       CRLF
                    fcb       $1B,$5B,$4A         ; CLEAR TO END OF SCREEN
                    fcc       "MY PLAY IS "
                    fcb       ENDST

; YOUR PLAY? - MES7
;       ORG     $56E
MES7                fdb       LNFDS
                    fcc       ". YOUR PLAY? "
                    fcb       ENDST

                    end
