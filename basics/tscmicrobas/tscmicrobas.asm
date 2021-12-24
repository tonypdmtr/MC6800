;*******************************************************************************
; MICRO BASIC PLUS SOURCE LISTING
;
; MICRO BASIC PLUS
; COPYRIGHT (C) 1976 BY
;
; TECHNICAL SYSTEMS CONSULTANTS
; BOX 2574
; W. LAFAYETTE INDIANA 47906
;
; Modified to run on Corsham Tech 6800 by
; Christopher "Zoggins" Mallery
;
; Modified for 68HC11 and ASM11 by
; Tony Papadimitriou <tonyp@acm.org>
;
; Original was found here: https://github.com/zoggins/MC6800
;*******************************************************************************

; EQUATES
STACK               equ       $A07F
PIAADR              equ       $8004
PFILBG              equ       $A002
PFILEN              equ       $A004
EXTERN              equ       $1F00               ; External routine
MONITR              equ       $E0E3
MONPC               equ       $A048
STKBOT              equ       $A000

;*******************************************************************************
                    #RAM                          ; TEMPORARY STORAGE
;*******************************************************************************

RNDM                rmb       4
BUFPNT              rmb       2
FORSTK              rmb       2
DIMPNT              rmb       2
XTEMP3              rmb       2
DATAST              rmb       2
DATAPT              rmb       2
TRYVAL              rmb       2
CRFLAG              rmb       1
QMFLAG              rmb       1
ROWVAR              rmb       1
ROWCON              rmb       1
COLCON              rmb       1
TABFLG              rmb       1
DIMFLG              rmb       1
RUNFLG              rmb       1
DATAFL              rmb       1
SUBCNT              rmb       1
LETFLG              rmb       1
FLDCNT              rmb       1
NXPNTR              rmb       2
XTEMP               rmb       2
XSAVE               rmb       2
XSAVE2              rmb       2
NUMCNT              rmb       1
NEGFLG              rmb       1
NOEXFL              rmb       1
EXTRA               rmb       2
COUNT               rmb       1
STKCNT              rmb       1
AUXCNT              rmb       1
SIGN                rmb       1
AXSIGN              rmb       1
OVFLBF              rmb       1
XTEMP2              rmb       2
XTEMP4              rmb       2
XTEMP5              rmb       2
CPX1                rmb       2
CPX2                rmb       2
STKEND              rmb       3
CHRCNT              rmb       1
OPSTAK              rmb       32
AC                  rmb       3
NUMBER              rmb       3
AX                  rmb       3
BUFFER              rmb       72

; LABLE TABLE

LBLTBL              rmb       78
STKTOP              rmb       2

;*******************************************************************************
; CONSTANTS
;*******************************************************************************

BACKSP              equ       $8
DELCOD              equ       $18
PRMPTC              equ       $21

;*******************************************************************************
                    #ROM      $0100               ; MAIN PROGRAM
;*******************************************************************************

START               jmp       MICBAS              ; JMP TO BEGIN
RESTRT              jmp       FILBUF

; EXTERNAL I-O ROUTINES

OUTEEE              jmp       $E1D1
INCH                jsr       $E1AC
BREAK               jmp       SHTCUT              ; # FOR MP-C BOARD USE JMP INTBRK
MEMEND              fdb       $1EFF

;*******************************************************************************
; KEYWORD AND JUMP TABLE
;*******************************************************************************

KEYTBL              fcc       'PRI'
                    fdb       PRINT

                    fcc       'INP'
                    fdb       INPUT

                    fcc       'IF '
                    fdb       IF

                    fcc       'LET'
LETADR              fdb       LET

                    fcc       'FOR'
                    fdb       FOR

                    fcc       'NEX'
                    fdb       NEXT

                    fcc       'GOT'
                    fdb       GOTO

                    fcc       'GOS'
                    fdb       GOSUB

                    fcc       'ON '
                    fdb       ONGOTO

                    fcc       'RET'
                    fdb       RETURN

                    fcc       'REA'
                    fdb       READ

                    fcc       'DAT'
                    fdb       DATA

                    fcc       'RES'
                    fdb       RESTOR

                    fcc       'DIM'
                    fdb       DIM

                    fcc       'EXT'
                    fdb       EXTRNL

                    fcc       'MON'
                    fdb       MONITR

                    fcc       'END'
                    fdb       FILBUF

                    fcc       'REM'
                    fdb       RUNEXC

                    fcc       'RUN'
                    fdb       RUN

                    fcc       'LIS'
                    fdb       LIST

                    fcc       'SCR'
                    fdb       MICBAS
                    fcb       0

FCTTBL              fcc       'RND'
                    fdb       EVAL88

                    fcc       'ABS'
                    fdb       EVAL85

                    fcc       'SGN'
                    fdb       EVAL86
                    fcb       0

;*******************************************************************************
; INITIALIZATION

CLRBEG              proc
                    ldx       #START
                    stx       XTEMP3              ; SAVE X
;                   bra       CLRBG2

;*******************************************************************************

CLRBG2              proc
                    ldx       #DATAST             ; SET START
                    bra       CLEAR               ; GO CLEAR

;*******************************************************************************

CLREND              proc
                    ldx       MEMEND              ; SET END
                    stx       XTEMP3              ; SAVE
                    ldx       ENDSTR
;                   bra       CLEAR

;*******************************************************************************

CLEAR               proc
                    clra                          ; CLEAR ACC.
CLEAR2              staa      ,x                  ; CLEAR BYTE
                    inx                           ; BUMP THE POINTER
                    cpx       XTEMP3              ; DONE?
                    bne       CLEAR2
                    rts                           ; RETURN

;*******************************************************************************

MICBAS              proc
                    bsr       CLRBEG              ; GO CLEAR
                    ldx       #STORSP
                    stx       ENDSTR              ; SET END STORAGE:
                    bsr       CLREND              ; GO CLEAR
;                   bra       FILBUF

;*******************************************************************************
; GET LINE INTO INPUT BUFFER

FILBUF              proc
                    ldx       #RESTRT
                    stx       MONPC               ; SET UP RETURN POINTER
                    lds       #STACK
                    ldx       #BUFFER
                    stx       XTEMP3              ; SAVE BOUND
                    bsr       CLRBG2
                    ldx       #ENDSTR             ; SET PUHCH LIMITS
                    stx       PFILBG
                    ldx       ,x                  ; SET END
                    stx       PFILEN
                    stx       DIMPNT
                    ldx       #BUFFER             ; POINT TO BUFFER
                    jsr       PCRLF               ; OUT A CR & LF
                    ldaa      #PRMPTC
                    jsr       OUTCH               ; OUTPUT PROMPT
FILBU2              jsr       INCHAR              ; GET A CHARACTER
                    beq       FILBUF
                    staa      ,x                  ; SAVE CHAR.
                    cmpa      #$0D                ; IS IT A C.R. ?
                    beq       FILBU6
                    inx                           ; BUMP THE POINTER
                    cpx       #BUFFER+72
                    bne       FILBU2              ; END OF BUFFER?
                    bra       FILBUF

FILBU6              ldx       #BUFFER             ; RESET POINTER
                    jsr       BCDCO1              ; LINE NO. CONV.
                    stx       XTEMP2              ; SAVE POINTER
                    jsr       FNDKEY              ; CHECK KEY WORD
                    tsta
                    bne       FILBU8              ; IF NONZERO THEN OK
                    ldx       BUFPNT              ; POINT TO BUFFER
                    ldaa      ,x                  ; GET CHARACTER
                    cmpa      #$D                 ; IS IT A C.R.?
                    bne       FILBU7
                    ldab      NOEXFL              ; DIR. EXECUTION?
                    beq       FILBUF
                    staa      CRFLAG              ; SET FLAG
                    bra       FILBU8              ; IT IS OK

FILBU7              jsr       TSTLET              ; LET?
                    beq       FILBU8
FILB75              ldaa      #$10
                    jmp       MISTAK              ; REPORT ERROR #0

FILBU8              ldaa      CHRCNT              ; GET CHAR. COUNT
                    suba      NUMCNT              ; SUB LINE # DIGITS
                    staa      CHRCNT              ; SAVE
                    ldab      NOEXFL              ; DIRECT EXECUTE ?
                    bne       STUFLN              ; IF NOT GO PUT LINE
                    jsr       PCRLF               ; OUTPUT C.R. L.F.
                    jmp       RUNEX4              ; GO TO ROUTINE

;*******************************************************************************
; PUT LINE IN PROGRAM STORAGE

STUFLN              proc
                    ldx       MEMEND
                    stx       CPX1
                    ldx       XTEMP2              ; SET POINTER
                    stx       BUFPNT              ; SAVE POINTER
                    bsr       FNDLIN              ; GO FIND LINE IN STORE
                    stx       XSAVE               ; SAVE POINTER
                    tstb      DID                 ; WE FIND IT?
                    bne       INSERT              ; IF NOT GO INSERT
;                   bra       REPLAC

;*******************************************************************************
; REPLACE EXISTING LINE WITH NEW ONE

REPLAC              incb                          ; INC THE COUNTER
                    ldaa      ,x                  ; GET A CHARACTER
                    inx                           ; BUMP THE POINTER
                    cmpa      #$D                 ; IS IT A C.R,?
                    bne       REPLAC
REPLA4              stab      OFSET2+1            ; SETUP OFFSET
                    ldaa      #$FF                ; GET COUNT
                    negb                          ; 2'S COMP. IT
                    bsr       ADJEND              ; GO FIX END PNTR
                    ldx       XSAVE               ; RESTORE THE POINTER
REPLA5              cpx       ENDSTR              ; END OF STORAGE?
                    beq       REPLA6
OFSET2              ldaa      ,x
                    staa      ,x                  ; MOVE A CHARACTER
                    inx                           ; BUMP THE POINTER
                    bra       REPLA5              ; REPEAT

REPLA6              ldx       XSAVE               ; RESTORE THE POINTER
;                   bra       INSERT

;*******************************************************************************
; INSERT A LINE INTO PROGRAM STORAGE

INSERT              proc
                    ldaa      CRFLAG              ; LONE C.R. ?
                    bne       INSER6
                    ldx       ENDSTR
                    ldab      CHRCNT              ; GET CHAR. COUNT
                    addb      #2                  ; BIAS FOR LINE NUM.
                    stab      OFFSET+1            ; SETUP OFFSET
                    bsr       ADJEND              ; FIX END PNTR
INSER2              cpx       XSAVE               ; DONE?
                    beq       INSER3
                    dex                           ; DEC THE POINTER
                    ldaa      ,x                  ; GET A CHAR,
OFFSET              staa      ,x
                    bra       INSER2              ; MOVE IT

INSER3              dex
                    jsr       PUTLB2              ; PUT LAB
                    inx:2                         ; BUMP THE POINTER
INSER4              stx       XSAVE               ; SAVE POINTER
                    ldx       BUFPNT
                    ldaa      ,x                  ; GET CHAR*
                    inx                           ; BUMP THE POINTER
                    stx       BUFPNT              ; SAVE
                    ldx       XSAVE               ; RESTOR PNTR
                    inx
                    staa      ,x                  ; SAVE IT
                    cmpa      #$D                 ; IS IT A C.R.?
                    bne       INSER4
INSER6              jmp       FILBUF              ; 60 TO MAIN LOOP

;*******************************************************************************
; ADJUST THE END OF PROGRAM POINTER

ADJEND              proc
                    addb      ENDSTR+1
                    adca      ENDSTR              ; ADD IN VALUE
                    stab      CPX2+1
                    staa      CPX2                ; SET END POINTER
                    jsr       CMPX1
                    bcc       ADJEN2
                    stab      ENDSTR+1
                    staa      ENDSTR              ; SAVE NEW POINTER
                    rts                           ; RETURN

ADJEN2              ldaa      #$90                ; SET ERROR
                    jmp       MISTAK

;*******************************************************************************
; TRY TO FIND LINE

FNDLIN              proc
                    ldaa      NUMBER+2
                    ldab      NUMBER+1
FINDLN              ldx       #STORSP             ; SETUP POINTER
FINDL1              cpx       ENDSTR              ; END OF STORAGE?
                    bne       FINDL4
FINDL2              incb
                    rts                           ; RETURN

FINDL4              cmpb      ,x                  ; CHECK M.S. DIGITS
                    bhi       FINDL6
                    bne       FINDL2
                    cmpa      1,x                 ; CHECK L.S, DIGITS
                    bhi       FINDL6
                    bne       FINDL2
                    clrb                          ; CEAR FLAG
                    rts                           ; RETURN

FINDL6              bsr       FNDCRT              ; GO FIND C.R,
                    inx                           ; BUMP THE POINTER
                    bra       FINDL1              ; REPEAT

;*******************************************************************************
; FIND A C,R, IN STORAGE

FNDCRT              proc
                    psha                          ; SAVE A
                    ldaa      #$D
FNDVAL              inx                           ; BUMP THE POINTER
                    cmpa      ,x                  ; TEST FOR C.R.
                    bne       FNDVAL
                    pula                          ; RESTORE A
                    rts                           ; RETURN

;*******************************************************************************
; INPUT

INCHAR              proc
                    jsr       INCH                ; GET THE CHAR.
                    cmpa      #BACKSP             ; IS IT A BACKSPACE?
                    bne       INCHR2
                    cpx       #BUFFER             ; BEGINNING OF BUF ?
                    beq       INCHR4
                    dex                           ; BACKUP ONE POS.
                    dec       CHRCNT              ; DEC CHAR. COUNT
                    bra       INCHAR

INCHR2              cmpa      #DELCOD             ; DELETE LINE ?
                    beq       INCHR4
                    inc       CHRCNT
INCHR4              rts                           ; RETURN

;*******************************************************************************
; PRINT CARRIAGE RETURN & LINEFEED

PCRLF               proc
                    stx       XSAVE               ; SAVE X REG
                    ldx       #CRLFST             ; POINT TO STRING
PDATA1              ldaa      ,x                  ; GET CHAR
                    cmpa      #4                  ; IS IT 4?
                    beq       PCRLF2
                    jsr       OUTCH               ; OUTPUT CHAR
                    inx                           ; BUMP THE POINTER
                    bra       PDATA1              ; REPEAT

PCRLF2              ldx       XSAVE               ; RESTORE X REG
                    clr       FLDCNT              ; ZERO FIELD COUNT
                    rts                           ; RETURN

CRLFST              fcb       $D,$A,0,0,0,0,4

;*******************************************************************************
; TEST FOR STATEMENT TERMINATOR

TSTTRM              proc
                    cmpa      #$D                 ; C,R,?
                    beq       TSTTR2
                    cmpa      #':'                ; COLON?
TSTTR2              rts                           ; RETURN

;*******************************************************************************
; CLEAR NUMBER THROUGH NUMBER+2

UPSCLR              proc
                    jsr       STAKUP
;                   bra       CLRNUM

;*******************************************************************************

CLRNUM              proc
                    clra
                    staa      NUMBER
                    staa      NUMBER+1
                    staa      NUMBER+2
                    rts

;*******************************************************************************
; CONVERT NUMBER TO PACKED BCD

BCDCON              proc
                    bsr       CLRNUM              ; CLEAR NUMBER
                    staa      NOEXFL
                    staa      NEGFLG
                    staa      NUMCNT
                    bsr       SKIPSP              ; SKIP SPACES
                    cmpa      #'+'                ; IS IT A +?
                    beq       BCDC01
                    cmpa      #'-'                ; IS IT A - ?
                    bne       BCDCO1
                    com       NEGFLG              ; SET FLAG
BCDC01              inx
BCDCO1              jsr       CLASS               ; GET A DIGIT
                    cmpb      #3                  ; IS IT A NUMBER?
                    beq       BCDCO2
                    ldaa      NEGFLG
                    jmp       FIXSIN              ; GO FIX UP THE SIGN

BCDCO2              inx                           ; BUMP THE POINTER
                    staa      NOEXFL              ; SET NO EXEC FLU
                    anda      #$0F                ; MASK OFF ASCII
                    ldab      #4                  ; SET COUNTER
BCDCO4              asl       NUMBER+2
                    rol       NUMBER+1
                    rol       NUMBER              ; SHIFT PREV. OVER
                    decb                          ; DEC THE COUNTER
                    bne       BCDCO4
                    adda      NUMBER+2
                    staa      NUMBER+2            ; SAVE NEW VALUE
                    inc       NUMCNT              ; INC NUMBER CNTR
                    bra       BCDCO1

;*******************************************************************************
; FIND NEXT BLOCK

NXTBLK              proc
                    ldx       BUFPNT              ; RESTORE POINTER
NXTBL4              ldaa      ,x                  ; GET A CHAR.
                    cmpa      #' '                ; IS IT A SPACE?
                    beq       SKIPSP
                    inx                           ; BUMP THE POINTER
                    bra       NXTBL4              ; REPEAT

;*******************************************************************************
; CONVERT AND SKIP

CONSKP              proc
                    bsr       BCDCON
                    dex
;                   bra       SKPSP0

;*******************************************************************************
; SKIP ALL SPACES

SKPSP0              proc
                    inx
SKIPSP              ldaa      ,x                  ; GET CHR FROM BUF
                    cmpa      #' '                ; IS IT A SPACE?
                    beq       SKPSP0
SKIPS4              rts                           ; RETURN

;*******************************************************************************
; FIND NEXT BLOCK NOT EXPECTING A SPACE

NXTSPC              proc
                    ldx       BUFPNT              ; SET POINTER
NXTSP4              jsr       CLASS               ; GO CLASSIFY
                    cmpb      #2                  ; IS IT A LETTER?
                    bne       SKIPSP
                    inx                           ; BUMP THE POINTER
                    bra       NXTSP4

;*******************************************************************************
; FIND KEY WORD IF POSSIBLE

FNDKEY              proc
                    bsr       SKIPSP              ; SKIP SPACES
                    stx       BUFPNT              ; SAVE THE POINTER
                    stx       XSAVE
                    ldx       #KEYTBL             ; POINT TO KEY WORDS
FNDKE2              ldab      #5
FNDKE4              cmpa      ,x                  ; TEST THE CHARACTER
                    bne       FNDKE6
                    stx       XTEMP3              ; SAVE POINTER
                    ldx       XSAVE
                    inx                           ; BUMP POINTER
                    ldaa      ,x                  ; GET CHAR.
                    stx       XSAVE
                    ldx       XTEMP3              ; REST. PNTR.
                    inx
                    decb
                    cmpb      #2
                    bne       FNDKE4              ; IF NOT DONE REPEAT
FNDKE5              rts                           ; RETURN

FNDKE6              inx                           ; BUMP THE COUNTER
                    decb
                    bne       FNDKE6
                    ldaa      ,x                  ; GET CHARACTER
                    beq       FNDKE5              ; IF ZERO, END OF LIST
                    stx       XTEMP3              ; SAVE POINTER
                    ldx       BUFPNT
                    stx       XSAVE
                    ldaa      ,x                  ; GET NEW CHAR.
                    ldx       XTEMP3              ; RESTORE POINTER
                    bra       FNDKE2              ; REPEAT

;*******************************************************************************
; OUTPUT A NUMBER FROM PACKED BCD BYTES

OUTBCD              proc
                    ldx       #NUMBER             ; SET POINTER
OUTBCI              ldab      #2                  ; SET COUNTER
                    clc
                    ldaa      ,x                  ; GET A WORD
                    bpl       OUTBC4              ; IF NOT NEG JMP AHEAD
                    ldaa      #'-'
                    jsr       OUTCH               ; OUTPUT A
                    inc       FLDCNT
                    bra       OUTBC4

OUTBC2              ldaa      ,x                  ; GET DIGITS
                    bita      #$F0                ; MASK
                    bcs       OUTBC3
                    beq       OUTBC4              ; JMP IF ZEROES
OUTBC3              bsr       OUTHL               ; OUTPUT A DIGIT
                    inc       FLDCNT
                    sec
OUTBC4              ldaa      ,x                  ; GET A DIGIT
                    bitb      #$FF                ; LAST DIGIT?
                    beq       OUTBC6
                    bita      #$0F                ; MASK
                    bcs       OUTBC6
                    beq       OUTBC8              ; JMP IF ZEROES
OUTBC6              bsr       OUTHR               ; OUTPUT A DIGIT
                    inc       FLDCNT
                    sec
OUTBC8              inx                           ; BUMP THE POINTER
                    decb                          ; DEC THE COUNTER
                    bpl       OUTBC2              ; REPEAT IF NOT DONE
                    rts                           ; RETURN

;*******************************************************************************
; LIST USERS PROGRAM

LIST                proc
                    bsr       NXTSPC              ; FIND NEXT
                    cmpa      #$D
                    beq       LIST3
                    jsr       BCDCON              ; GET LINE NUM
                    stx       BUFPNT              ; SAVE POINTER
                    jsr       FNDLIN              ; FIND LINE
                    stx       XSAVE               ; SAVE IT
                    jsr       NXTSPC
                    cmpa      #$D                 ; C.R.?
                    bne       LIST1
                    inc       SUBCNT              ; SET TO 1
                    bra       LIST2

LIST1               inx                           ; BUMP THE POINTER
                    jsr       SKIPSP
                    jsr       BCDCON              ; GET COUNT
                    ldaa      NUMBER+2
                    staa      SUBCNT              ; SAVE IT
LIST2               ldx       XSAVE               ; POINT TO LINE
                    bra       LIST4

LIST3               ldx       #STORSP             ; SET POINTER
LIST4               cpx       ENDSTR              ; END OF STORAGE?
                    beq       LIST8
                    jsr       PCRLF               ; OUTPUT A
                    ldab      #1                  ; SETUP COUNTER
                    clc
                    bsr       OUTBC2              ; OUT LINE NUMBER
LIST5               ldaa      ,x                  ; GET A CHARACTER
                    cmpa      #$D                 ; IS IT A C.R.?
                    beq       LIST6
                    bsr       OUTCH               ; OUTPUT CHARACTER
                    inx                           ; BUMP THE POINTER
                    bra       LIST5               ; REPEAT

LIST6               inx                           ; BUMP THE POINTER
                    ldaa      SUBCNT              ; GET COUNT
                    beq       LIST4
                    adda      #$99                ; DEC THE COUNT
                    daa
                    beq       LIST8
                    staa      SUBCNT              ; SAVE
                    bra       LIST4

LIST8               jmp       FILBUF

;*******************************************************************************

OUTHL               proc
                    lsra:4                        ; MOVE TO BOTTOM
;                   bra       OUTHR

;*******************************************************************************

OUTHR               proc
                    anda      #$0F                ; MASK
                    adda      #$30                ; BIAS
;                   bra       OUTCH

;*******************************************************************************

OUTCH               proc
                    jsr       BREAK               ; CHECK FOR BREAK
                    jmp       OUTEEE              ; GO PRINT

;*******************************************************************************
; INTERNAL BREAK ROUTINE

INTBRK              proc
                    psha
                    ldaa      PIAADR              ; CHECK
                    bpl       BREAK2
                    pula                          ; GET CHAR
SHTCUT              rts                           ; RETURN

BREAK2              ldaa      PIAADR
                    bpl       BREAK2
                    ldaa      #$99                ; SET ERROR

;*******************************************************************************
; OUTPUT ERROR MESSAGE

MISTAK              proc
                    psha                          ; SAVE A
                    jsr       PCRLF               ; OUTPUT A CR & LF
MISTA1              ldx       #ERRSTR             ; POINT TO ERROR STRING
                    jsr       PDATA1              ; OUTPUT IT
                    pula                          ; RESTORE A
                    psha                          ; SAVE A
                    bsr       OUTHL               ; OUTPUT DIGIT
MISTA2              pula                          ; RESTORE A
                    bsr       OUTHR               ; OUT 1'S DIGIT
                    ldab      RUNFLG              ; RUNNING?
                    bne       RUNER1
MISTA4              jmp       FILBUF

RUNER1              ldx       #ERSTR2             ; POINT TO STRING
                    jsr       PDATA1              ; OUTPUT IT
                    ldx       BUFPNT              ; SET POINTER
RUNER2              dex                           ; DEC THE POINTER
                    cpx       #STORSP             ; BEGINNING?
                    beq       RUNER4
                    ldaa      ,x                  ; GET CHAR
                    cmpa      #$D                 ; C.R.?
                    bne       RUNER2
                    inx                           ; BUMP THE POINTER
RUNER4              ldab      #1
                    clc
                    jsr       OUTBC2              ; OUT LINE NUM.
                    bra       MISTA4

ERRSTR              fcb       7
                    fcc       'ERROR #'
                    fcb       4

ERSTR2              fcc       ' AT '
                    fcb       4

;*******************************************************************************
; PRINT ROUTINE

PRINT               proc
                    jsr       NXTSPC              ; FIND NEXT BLOCK
PRINT0              jsr       TSTTRM
                    bne       FIELD1
                    jmp       PRINT8

FIELD1              clr       CRFLAG
                    cmpa      #','                ; IS IT A ","
                    bne       PRINT2
                    ldab      FLDCNT              ; GET COUNT
FIELD2              ldaa      #' '                ; SPACE
                    bsr       OUTCH               ; OUTPUT A SPACE
                    incb
                    bitb      #7                  ; END OF FIELD?
                    bne       FIELD2
                    cmpb      #$47                ; END OF LINE?
                    bhi       FIELD3
                    stab      FLDCNT              ; SAVE FIELD INFO
                    bra       PRINT1

FIELD3              jsr       PCRLF               ; OUT A C.R. & L.F.
PRINT1              inc       CRFLAG              ; SET FLAG
                    inx                           ; BUMP THE POINTER
                    jsr       SKIPSP
                    bra       PRINT0

PRINT2              cmpa      #';'                ; IS IT A ";"
                    beq       PRINT1
                    cmpa      #'"'                ; IS IT A QUOTE?
                    bne       PRINT4
                    inx                           ; BUMP THE POINTER
                    bsr       PSTRNG              ; OUTPUT STRING
                    bra       PRINT6

PRINT4              clr       TABFLG              ; CLEAR FLAG
                    cmpa      #'T'                ; IS IT A T?
                    bne       PRIN45
                    staa      TABFLG              ; SET FLAG
                    ldaa      #'A'
                    bra       PRIN47

PRIN45              cmpa      #'S'                ; IS IT A S?
                    bne       PRIN55
                    ldaa      #'P'
PRIN47              cmpa      1,x
                    bne       PRIN55
                    jsr       NXTSP4              ; FIND NEXT
                    jsr       EXPR                ; EVALUATE
                    jsr       BINCON              ; CONVERT
                    ldab      NUMBER+2
                    beq       PRINT6
                    ldaa      TABFLG              ; CHECK FLAG
                    beq       PRINT5
                    decb
                    cmpb      FLDCNT              ; CHECK COUNT
                    bls       PRINT6
                    bra       PRIN51

PRINT5              addb      FLDCNT
PRIN51              ldaa      #' '                ; SPACE
                    jsr       OUTCH               ; OUTPUT SPACE
                    inc       FLDCNT              ; BUMP COUNTER
                    cmpb      FLDCNT
                    bne       PRIN51              ; REPEAT
PRIN52              bra       PRINT6

PRIN55              jsr       EXPR                ; EVAL EXPRESSION
                    stx       XSAVE               ; SAVE POINTER
                    jsr       OUTBCD              ; OUTPUT VALUE
                    ldx       XSAVE               ; RESTORE
PRINT6              jsr       SKYCLS
                    decb
                    bne       PRINT7              ; CHECK FOR ERROR
                    jmp       PRINT0

PRINT7              ldaa      #$31
                    jmp       MISTAK

PRINT8              tst       CRFLAG              ; C.R. ?
                    bne       PRINT9
                    jsr       PCRLF               ; OUTPUT C.R. L.F
PRINT9              jmp       RUNEXC

;*******************************************************************************
; PRINT STRING ROUTINE

PSTRNG              proc
                    ldaa      ,x                  ; GET A CHAR.
                    cmpa      #'"'                ; IS I T A QUOTE?
                    beq       Cont@@
                    jsr       TSTTRM              ; IS IT A C.R.?
                    beq       Fail@@
                    jsr       OUTCH               ; OUTPUT CHARACTER
                    inc       FLDCNT              ; BUMP FIELD CNT
                    inx                           ; BUMP THE POINTER
                    bra       PSTRNG              ; REPEAT

Cont@@              inx
                    jmp       SKIPSP

Fail@@              ldaa      #$32
                    jmp       MISTAK              ; REPORT ERROR

;*******************************************************************************
; FIND LABLE ROUTINE

FNDVAR              proc
                    stx       BUFPNT              ; SAVE POINTER
                    jsr       CLASS1              ; GO CLASSIFY CHAR.
                    cmpb      #2                  ; CHECK FOR LETTER
                    bne       FNDL25              ; ERROR
                    clr       XTEMP
                    tab                           ; SAVE LABLE
                    asla                          ; MULT IT BY 2
                    aba                           ; ADD IT
                    suba      #$13
                    staa      XTEMP+1
                    ldx       XTEMP               ; POINT TO IT
                    rts                           ; RETURN

;*******************************************************************************
; FIND DIMENSIONED VARIABLE

FNDLB0              proc
                    ldaa      ,x
FNDLBL              inx                           ; ADVANCE POINTER
                    clr       DIMFLG
                    bsr       FNDVAR              ; GO FIND VAR.
                    clrb
                    ldaa      ,x                  ; GET CHAR.
                    cmpa      #$0A                ; CHECK FOR 1 DIM
                    beq       FNDLB2
                    cmpa      #$0B                ; CHECK IF 2 DIM
                    beq       FNDLB1
                    rts

FNDLB1              incb                          ; SET FLAG-2 DIM
FNDLB2              ldaa      1,x                 ; SET POINTER
                    psha
                    ldaa      2,x
                    psha
                    pshb                          ; SAVE B
                    jsr       NXTSPC              ; FIND NEXT
                    pulb
                    cmpa      #'('                ; IS IT A PAREN?
FNDL25              bne       FNDLB9
                    tstb
                    beq       FNDLB3
                    inx
                    jsr       EXPRO               ; GO EVALUATE
                    ldaa      NUMBER+2            ; GET RESULT
                    psha                          ; SAVE IT
                    jsr       STAKDN              ; RESTORE
                    jsr       NXTSPC              ; FIND NEXT
                    cmpa      #','                ; IS IT A COMMA?
                    bne       FNDLB9
                    bra       FNDLB4

FNDLB3              clra
                    psha                          ; SET ROWV
FNDLB4              inca
                    staa      DIMFLG              ; SET FLAG
                    inx
                    jsr       EXPRO
                    inx
                    stx       BUFPNT              ; SAVE POINTER
                    pula
                    staa      ROWVAR              ; SAVE
                    pula
                    staa      XTEMP+1             ; SAVE
                    pula
                    staa      XTEMP               ; SAVE
                    ldx       XTEMP               ; SET POINTER
                    ldaa      ,x                  ; GET CHAR
                    staa      COLCON              ; SAVE IT
                    inx                           ; BUMP THE POINTER
                    inx
                    stx       XTEMP
                    jsr       UPSCLR
                    ldaa      ROWVAR              ; GET VAR.
                    ldx       XTEMP
                    dex                           ; DEC POINTER
                    cmpa      ,x                  ; CHECK
                    bhi       FNDLB9
                    staa      NUMBER+2
                    jsr       UPSCLR              ; PUSH STACK
                    ldaa      COLCON              ; GET CONST,
                    cmpa      AC-1                ; CHECK
                    beq       FNDL45
                    bls       FNDLB9              ; ERROR!
FNDL45              adda      #1
                    daa                           ; BIAS IT
                    staa      NUMBER+2
                    jsr       MULT                ; GO MULTIPLY
                    jsr       ADD                 ; GO ADD
FNDLB5              bsr       TIMTHR

;*******************************************************************************
; ROUTINE TO ADD VALUE TO X-REG.

ADDX                proc
                    ldaa      XTEMP               ; GET M.S.BYTE
                    ldab      XTEMP+1
                    addb      NUMBER+2
                    adca      NUMBER+1
                    staa      XTEMP               ; SAVE SUM
                    stab      XTEMP+1
                    jsr       STAKDN
                    ldx       XTEMP               ; SET POINTER
                    clr       DIMFLG              ; RESTORE FLAG
                    rts                           ; RETURN

FNDLB9              ldaa      #$14                ; SET ERROR
                    jmp       MISTAK              ; GO REPORT

;*******************************************************************************
; ROUTINE TO MULTIPLY BY 3

TIMTHR              proc
                    jsr       UPSCLR
                    ldaa      #$3                 ; SET MULTIPLIER
                    staa      NUMBER+2
                    jsr       MULT                ; GO MULTIPLY
;                   bra       BINCON

;*******************************************************************************
; BCD TO BINARY CONVERT.

BINCON              ldaa      NUMBER+2            ; GET LS BYTE
                    psha                          ; SAVE
                    ldaa      NUMBER+1
                    psha                          ; SAVE
                    clrb
                    stab      NUMBER+1
                    stab      NUMBER+2            ; INITIALIZE
                    ldaa      NUMBER
                    bsr       ADSHF1              ; ADDAND SHIFT
                    pula
                    psha
                    bsr       ADSHF0              ; GO ADD IN AND SHIFT
                    pula                          ; GET MS BYTE AGAIN
                    bsr       ADSHF1              ; GO ADD IN AND SHIFT
                    pula                          ; GET LS BYTE
                    psha
                    bsr       ADSHF0
                    pula
                    bra       ADDIN               ; G0 ADD IN ONES

ADSHF0              lsra
                    lsra
                    lsra
                    lsra                          ; MOVE TO LS HALF
ADSHF1              bsr       ADDIN               ; GO ADD IN
                    ldab      NUMBER+1
                    asla
                    rolb                          ; MULT BY 2
                    pshb
                    psha                          ; SAVE
                    asla
                    rolb
                    asla
                    rolb                          ; MULT BY 4, =*8
                    staa      NUMBER+2
                    pula
                    stab      NUMBER+1
                    bsr       ADDIN1              ; GO ADD IN
                    pula
                    adda      NUMBER+1
                    staa      NUMBER+1            ; MULTIPLY BY TEN
                    rts

ADDIN               anda      #$0F                ; MASK
ADDIN1              adda      NUMBER+2
                    staa      NUMBER+2
                    bcc       ADDIN2              ; CHECK FOR CARRY
                    inc       NUMBER+1
ADDIN2              rts

;*******************************************************************************
; PUT LABLE ROUTINE

PUTLBL              proc
                    ldaa      NUMBER
                    staa      ,x                  ; PUT M.S. BYTE
PUTLB2              ldaa      NUMBER              ; +1
                    staa      1,x                 ; PUT NEXT
                    ldaa      NUMBER+2
                    staa      2,x                 ; PUT L.S. BYTE
                    rts                           ; RETURN

;*******************************************************************************
; DIMENSION

DIM                 proc
                    ldx       FORSTK              ; SET BOUNDS
                    stx       CPX1
                    jsr       NXTSPC
DIMN                jsr       SKIPSP              ; CLASSIFY
                    jsr       FNDVAR
                    stx       XTEMP3              ; SAVE IT
                    jsr       NXTSPC              ; GET TO NEXT
                    cmpa      #'('                ; IS IT A PARENT
                    bne       DIM9
DIM01               inx                           ; BUMP THE POINTER
                    jsr       CONSKP              ; CONVERT DIM
                    cmpa      #')'                ; IS IT A PAREN
                    bne       DIM1
                    clra
                    clrb
                    psha                          ; SAVE IT
                    bra       DIM2

DIM1                cmpa      #','                ; COMMA?
                    bne       DIM9                ; ERROR!
                    ldaa      NUMBER+2
                    beq       DIM9
                    psha                          ; SAVE
                    inx                           ; BUMP THE POINTER
                    jsr       CONSKP              ; CONVERT
                    ldab      #1
                    cmpa      #')'                ; PAREN?
                    beq       DIM2
DIM9                ldaa      #$40                ; SET ERROR
                    jmp       MISTAK              ; REPORT

DIM2                ldaa      NUMBER+2
                    beq       DIM9
                    psha                          ; SAVE
                    stx       BUFPNT              ; SAVE POINTER
                    ldx       XTEMP3              ; SET X
                    ldaa      #$0A
                    aba                           ; SET MARKER
                    staa      ,x                  ; SAVE IT
                    ldaa      DIMPNT              ; GET POINTER
                    staa      1,x                 ; SAVE IT
                    ldaa      DIMPNT+1
                    staa      2,x
                    ldx       DIMPNT              ; SET POINTER
                    pula
                    staa      ,x                  ; SAVE 1ST DIM
                    inx                           ; BUMP THE POINTER
                    pulb
                    stab      ,x                  ; SAVE 2ND DIM
                    inx
                    stx       XTEMP               ; SAVE POINTER
                    adda      #1
                    daa                           ; BIAS
                    psha
                    tba
                    adda      #1                  ; BIAS
                    daa                           ; ADJUST
                    tab                           ; SAVE
                    jsr       CLRNUM              ; CLEAR STORAGE
                    stab      NUMBER+2
                    jsr       UPSCLR              ; GO CLEAR
                    pula
                    staa      NUMBER+2
                    jsr       MULT                ; MULTIPLY
                    jsr       FNDLB5              ; GO FIX X
                    jsr       CMPX                ; TEST BOUNDS
                    bls       DIM5
                    jmp       ADJEN2

DIM5                stx       DIMPNT              ; SAVE RESULT
                    ldx       BUFPNT              ; RESTORE F'NTR
                    inx
                    jsr       SKIPSP              ; SKIP SPACES
                    jsr       TSTTRM
                    beq       RUNEXC
                    inx                           ; BUMP THE POINTER
                    jmp       DIMN

;*******************************************************************************
; EXTERNAL ROUTINE JUMP

EXTRNL              proc
                    jsr       EXTERN              ; GO TO IT
;                   bra       RUNEXC

;*******************************************************************************
; RUN EXECUTIVE

RUNEXC              proc
                    clra
                    staa      CRFLAG
                    staa      LETFLG
                    staa      DIMFLG
                    staa      STKCNT
                    ldaa      RUNFLG              ; RUN MODE?
                    bne       RUNEX0
RUNEXA              jmp       FILBUF

RUNEX0              ldx       BUFPNT              ; SET POINTER
RUNE05              ldaa      #$D
                    ldab      #':'                ; SETUP TERMINATORS
RUNEX1              cmpa      ,x                  ; C.R. ?
                    beq       RUNEX2
                    cmpb      ,x                  ; IS IT A ':' ?
                    beq       RUNE27
                    inx                           ; BUMP THE POINTER
                    bra       RUNEX1              ; REPEAT

RUNEX2              inx
RUNE22              cpx       ENDSTR              ; END OF STORAGE?
                    beq       RUNEXA
RUNE25              inx                           ; BUMP THE POINTER
RUNE27              inx
                    jsr       BREAK               ; GO CHECK BREAK
RUNEX3              jsr       FNDKEY              ; FIND KEY WORD
                    tsta
                    bne       RUNEX4
                    ldx       BUFPNT              ; SET POINTER
                    bsr       TSTLET
                    beq       RUNEX4
                    ldaa      #$10
RUNE35              jmp       MISTAK

RUNEX4              ldx       ,x
                    jmp       ,x                  ; GO TO ROUTINE

;*******************************************************************************
; TEST FOR IMPLIED LET

TSTLET              proc
                    jsr       CLASS               ; CHECK CHAR.
                    cmpb      #2                  ; LETTER?
                    bne       TSTLE2
                    inx                           ; BUMP THE POINTER
                    jsr       SKIPSP              ; SKIP SPACES
                    cmpa      #'='                ; EQUALS?
                    beq       TSTLE1
                    cmpa      #'('                ; LEFT PARENT
                    bne       TSTLE2
TSTLE1              ldx       #LETADR             ; SET POINTER
                    staa      LETFLG              ; SET FLAG
                    clrb
TSTLE2              rts

;*******************************************************************************
; RUN ROUTINE

RUN                 proc
                    jsr       CLRBEG
                    jsr       CLREND
                    ldx       MEMEND
                    stx       FORSTK
                    ldx       #STORSP             ; SET POINTER
                    inc       RUNFLG
                    bra       RUNE22

;*******************************************************************************
; LET ROUTINE

LET                 proc
                    ldx       BUFPNT
                    ldaa      LETFLG              ; TEST FLAG
                    bne       LET2
                    jsr       NXTBLK              ; FIND NEXT
LET2                jsr       EXPEQU
                    bra       RUNEXC

;*******************************************************************************
; GOTO ROUTINE

GOTO                proc
                    jsr       NXTSPC              ; FIND BLOCK
GOTO1               jsr       EXPR                ; GO EVALUATE
GOTO2               jsr       FNDLIN              ; GO FIND LINE
GOTO3               tstb      FIND?
                    beq       GOTO5
                    ldaa      #$16                ; SET ERROR
GOTO4               jmp       MISTAK              ; REPORT

GOTO5               incb
                    stab      RUNFLG              ; SET RUN FLAG
                    bra       RUNE22

;*******************************************************************************
; INPUT ROUTINE

INPUT               proc
                    jsr       NXTSPC              ; FIND NEXT
INPUT0              clr       QMFLAG              ; CLEAR FLAG
INPUT1              jsr       SKIPSP              ; SKIP SPACES
                    cmpa      #'"'                ; IS IT A QUOTE?
                    bne       INPUT2
                    inx                           ; BUMP THE POINTER
                    jsr       PSTRNG              ; OUTPUT STRING
                    bra       INPUT6

INPUT2              jsr       FNDLBL              ; FIND LABLE
                    stx       XTEMP4              ; SAVE POINTER
INPUT3              ldx       #BUFFER             ; SET POINTER
                    ldaa      QMFLAG              ; TEST FLAG
                    bne       INPUT4
                    ldaa      #'?'
                    staa      QMFLAG              ; SET FLAG
                    jsr       OUTCH               ; OUT A ?
INPUT4              jsr       INCH                ; GET A DIGIT
                    cmpa      #DELCOD             ; DELETE?
                    bne       INPU45
                    clr       QMFLAG
                    bra       INPUT3

INPU45              staa      ,x                  ; SAVE IT
                    inx
                    cmpa      #','                ; 1S IT COMMA?
                    beq       INPUT5
                    cmpa      #$D                 ; IS IT A C.R.?
                    bne       INPUT4
                    staa      CRFLAG              ; SET FLAG
                    jsr       PCRLF               ; OUTPUT A CR & LF
INPUT5              ldx       #BUFFER             ; SET POINTER
                    jsr       BCDCON              ; GO CNVRT NUM.
                    ldx       XTEMP4
                    bsr       LABLS2
                    stx       BUFPNT              ; SAVE POINTER
INPUT6              cmpa      #','                ; IS IT A COMMA?
                    bne       INPUT7
                    inx
                    ldaa      CRFLAG              ; TEST FLAG
                    beq       INPUT1
                    bra       INPUT0

INPUT7              jsr       TSTTRM
                    bne       INPUT9
INPU72              ldaa      CRFLAG              ; TEST FLAG
                    beq       INPUT8
INPU75              jmp       RUNEXC

INPUT8              jsr       INCH                ; GET CHAR.
                    cmpa      #$D                 ; C.R.?
                    bne       INPUT8
                    jsr       PCRLF
                    bra       INPU75

INPUT9              ldaa      #$45
                    jmp       MISTAK              ; REPORT ERROR

;*******************************************************************************
; GET AND PUT LABLE

LABLES              proc
                    jsr       FNDLBL              ; GO FIND IT
LABLS2              jsr       PUTLBL              ; GO PUT IT
                    jmp       NXTSPC              ; GET TO NEXT SET

;*******************************************************************************
; DATA ROUTINE

DATA                proc
                    ldaa      RUNFLG              ; RUNNING?
                    beq       READ6
                    jsr       NXTSPC              ; FIND NEXT
                    staa      DATAFL              ; SET DATA FLAG
                    stx       DATAST              ; SET POINTER
                    stx       DATAPT
                    bra       READ6               ; RETURN

;*******************************************************************************
; READ DATA ROUTINE

READ                proc
                    ldaa      RUNFLG              ; RUNNING?
                    beq       READ6
                    ldaa      DATAFL              ; CHECK FLAG
                    beq       READ8
                    jsr       NXTBLK              ; GET NEXT
READ2               jsr       SKIPSP              ; GO CLASSIFY
                    jsr       FNDLBL
                    stx       XTEMP4
                    ldx       BUFPNT
                    stx       XTEMP5              ; SAVE IT
                    ldx       DATAPT              ; GET DATA PNTR
                    jsr       EXPR                ; GET DATA
                    ldaa      ,x                  ; GET CHAR.
                    jsr       TSTTRM              ; TEST IT
                    bne       READ25
                    ldx       DATAST              ; SET POINTER
                    bra       READ3

READ25              inx                           ; BUMP THE POINTER
READ3               stx       DATAPT
                    ldx       XTEMP5
                    stx       BUFPNT
                    ldx       XTEMP4
                    bsr       LABLS2
                    cmpa      #','                ; IS IT A COMMA?
                    bne       READ4
                    inx
                    bra       READ2               ; REPEAT

READ4               jsr       TSTTRM
                    bne       READ8               ; ERROR
READ6               jmp       RUNEXC              ; RETURN

READ8               ldaa      #$51
                    jmp       MISTAK

;*******************************************************************************
; RESTORE DATA STRING

RESTOR              proc
                    stx       XSAVE               ; SAVE POINTER
                    ldx       DATAST
                    stx       DATAPT              ; FIX DATA PNTR
                    ldx       XSAVE               ; RESTORE POINTER
                    bra       READ6

;*******************************************************************************
; ON GOTO ROUTINE

ONGOTO              proc
                    jsr       NXTBLK              ; FIND NEXT BLOCK
                    jsr       EXPR                ; EVAL. EXPR.
                    ldaa      NUMBER+2
                    anda      #$0F                ; MASK L.S. DIGIT
                    psha                          ; SAVE A
                    clr       CRFLAG
                    inx                           ; BUMP THE POINTER
                    inx
                    ldaa      ,x                  ; GET CHAR
                    cmpa      #'T'                ; IS IT A "T"?
                    beq       ONGOT0
                    staa      CRFLAG              ; SET FLAG
ONGOT0              jsr       NXTBL4              ; GET NEXT
                    stx       XSAVE               ; SAVE X
                    pula                          ; RESTORE A
ONGOT1              deca
                    beq       ONGOT4
ONGOT2              ldab      ,x                  ; GET A CHAR,
                    inx                           ; BUMP THE POINTER
                    cmpb      #','                ; IS IT A COMMA?
                    bne       ONGOT3
                    stx       XSAVE               ; SAVE THE POINTER
                    bra       ONGOT1              ; REPEAT

ONGOT3              cmpb      #$D                 ; C^R^ ?
                    bne       ONGOT2
                    ldx       XSAVE               ; RESTORE POINTER
ONGOT4              ldab      CRFLAG              ; CHECK FLAG
                    beq       ONGOT6
                    jmp       GOSUB2
ONGOT6              jmp       GOTO1

;*******************************************************************************
; ROUTINE

IF                  proc
                    jsr       NXTSPC              ; FIND NEXT
                    jsr       EXPR                ; EUAL EXPR
                    ldaa      ,x                  ; GET CHAR
                    bsr       CLSREL              ; REL OPERATOR?
                    bne       IF9                 ; ERROR!
                    psha                          ; SAVE A
                    ldaa      1,x                 ; GET CHAR
                    bsr       CLSREL              ; REL OP?
                    pula                          ; RESTORE A
                    bne       IF1
                    ldab      1,x
                    aba                           ; FORM REL CODE
                    inx                           ; BUMP THE POINTER
IF1                 inx
                    psha                          ; SAVE A
                    jsr       EXPR                ; EVAL EXPR
                    pula
                    anda      #$0F                ; MASK
                    suba      #9                  ; BIAS IT
                    bmi       IF9                 ; ERROR?
                    asla                          ; TIMES FOUR
                    asla
                    staa      OFSET3+1
                    jsr       SUB                 ; GO COMPARE
                    jsr       ZCHK                ; SET CC REG
OFSET3              bra       *

BRATBL              ble       IF4                 ; BRANCH TABLE
                    bra       IF8

                    bne       IF4
                    bra       IF8

                    bge       IF4
                    bra       IF8

                    blt       IF4
                    bra       IF8

                    beq       IF4
                    bra       IF8

                    bgt       IF4
                    bra       IF8

                    bra       IF9                 ; ERROR!

IF4                 ldx       BUFPNT              ; SET POINTER
                    ldaa      ,x                  ; GET CHAR
                    cmpa      #'T'                ; IS IT A "T"?
                    bne       IF6
                    jsr       NXTSPC
                    stx       BUFPNT              ; SAVE POINTER
                    jsr       CLASS1              ; GO CLASSIFY
                    cmpb      #3                  ; IS IT A NUMBER?
                    bne       IF6
                    jmp       GOTO1               ; GO TO GOTO
IF6                 jmp       RUNEX3
IF8                 jmp       RUNEXC              ; GO PROCESS CMND

IF9                 ldaa      #$62                ; SET ERROR
                    jmp       MISTAK

;*******************************************************************************
; CLASSIFY RELATIONAL OPERATION

CLSREL              proc
                    cmpa      #$3B
                    bls       CLSRE5
                    cmpa      #$3E                ; CHECK CHAR
                    bhi       CLSRE5
                    clrb                          ; CLEAR FLAG
                    rts                           ; RETURN

CLSRE5              incb                          ; SET FLAG
                    rts                           ; RETURN

;*******************************************************************************
; GOSUB ROUTINE

GOSUB               proc
                    ldab      RUNFLG
                    beq       IF8
                    jsr       NXTSPC              ; FIND NEXT
GOSUB2              inc       SUBCNT
                    jsr       EXPR                ; EVALUATE EXPR
                    dex
                    jsr       FNDCRT              ; FIND C.R.
                    inx                           ; BUMP THE POINTER
                    ldaa      ,x                  ; GET LINE NO
                    psha
                    ldaa      1,x
                    psha                          ; SAVE AS RET. ADD.
                    sts       CPX1                ; SAVE SP
                    ldx       #STKBOT+35
                    jsr       CMPX                ; CHECK BOUNDS
                    bls       GOSUB4
                    jmp       ADJEN2              ; RPT OVFL

GOSUB4              jmp       GOTO2

;*******************************************************************************
; RETURN ROUTINE

RETURN              proc
                    ldaa      #$73
                    dec       SUBCNT              ; DEC COUNTER
                    bpl       RETUR2
                    jmp       MISTAK              ; ERROR!

RETUR2              pula                          ; GET RET. ADD.
                    pulb
                    jsr       FINDLN              ; GO FIND LINE
                    jmp       GOTO3

;*******************************************************************************
; EXPRESSION EQUATE

EXPEQU              proc
                    jsr       FNDLB0              ; FIND LABLE
                    stx       XTEMP4              ; SAVE
                    jsr       NXTSPC
                    inx
                    jsr       EXPR                ; GO EVALUATE
                    ldx       XTEMP4              ; GET POINTER
                    jmp       PUTLBL              ; INSTALL

;*******************************************************************************
; FOR ROUTINE

FOR                 proc
                    jsr       NXTBLK              ; FIND NEXT
                    psha
                    bsr       EXPEQU
                    ldx       DIMPNT
                    stx       CPX1
                    ldx       FORSTK
                    pula
                    staa      ,x
                    ldaa      BUFPNT+1
                    dex                           ; DEC THE POINTER
                    staa      ,x
                    ldaa      BUFPNT              ; SET UP INDEX
                    dex
                    staa      ,x
                    dex
                    jsr       CMPX                ; CHECK FOR OVFLW
                    bhi       FOR5
                    jmp       ADJEN2

FOR5                stx       FORSTK              ; SAVE POINTER
                    jmp       RUNEXC

;*******************************************************************************
; NEXT ROUTINE

NEXT                proc
                    jsr       NXTBLK              ; FIND NEXT
                    stx       NXPNTR
                    ldx       FORSTK              ; SET POINTER
NEXT1               cpx       MEMEND              ; OVFLW?
                    bne       NEXT2
                    ldx       BUFPNT              ; RESTORE PNTR
                    bra       NEXT9               ; ERROR!

NEXT2               inx                           ; FIXUP POINTER
                    inx
                    inx
                    cmpa      ,x                  ; CHECK
                    bne       NEXT1
                    dex                           ; FIX POINTER
                    dex
                    dex
                    stx       FORSTK
                    inx
                    ldx       ,x
                    stx       BUFPNT              ; SAVE IT
                    jsr       FNDLBL              ; FIND LABLE
                    stx       XTEMP4              ; SAVE IT
                    jsr       NXTSPC              ; FIND NEXT
                    bsr       EXPR                ; EVALUATE
                    jsr       STAKUP
                    ldx       XTEMP4              ; RESTORE PNTR
                    jsr       GETVAL              ; GET LABLE VALUE
                    ldx       BUFPNT
                    ldaa      ,x                  ; GET CHAR
                    cmpa      #'S'                ; IS IT STEP?
                    beq       NEXT4
                    jsr       UPSCLR
                    inca
                    staa      NUMBER+2
                    bra       NEXT5

NEXT4               jsr       NXTSP4
                    bsr       EXPR
                    ldaa      NUMBER
                    staa      LETFLG              ; SHOW NEG.
NEXT5               jsr       ADD                 ; GO ADD IN STEP
                    ldx       #TRYVAL             ; SET POINTER
                    jsr       PUTLBL              ; SAVE LABLE
                    jsr       SUB                 ; COMPARE
                    jsr       ZCHK                ; SET CC REG
                    ldab      LETFLG              ; CHK FLAG
                    bmi       NEXT6
                    tap                           ; SET CC
                    bge       NEXT8
                    bra       NEXT7

NEXT6               tap                           ; SET CC
                    ble       NEXT8
NEXT7               ldx       FORSTK
                    inx                           ; FIXUP PNTR
                    inx
                    inx
                    stx       FORSTK              ; SAVE IT
                    ldx       NXPNTR
                    stx       BUFPNT              ; SAVE
                    bra       NEXT85

NEXT8               ldx       #TRYVAL
                    jsr       GETVAL
                    ldx       XTEMP4
                    jsr       PUTLBL
NEXT85              jmp       RUNEXC

NEXT9               ldaa      #$81                ; SET ERROR
NEXTIO              jmp       MISTAK

;*******************************************************************************
; EXPRESSION HANDLER

EXPR                proc
                    clr       STKCNT              ; SET COUNT = 0
EXPRO               ldaa      STKCNT
                    staa      AUXCNT
                    bsr       EVAL
                    tsta                          ; CHECK FOR ERROR
                    bne       NEXTIO
EXPR1               rts                           ; RETURN

;*******************************************************************************
; EVALUATE AN ALGEBRAIC STRING

EVAL                proc
                    sts       STKTOP              ; SAVE SP TOP
EVA0A               jsr       SKYCLS
                    stx       BUFPNT
                    cmpb      #1                  ; SEE IF EMPTY EXPRESSION
                    bne       EVAL0
                    ldaa      #$21
                    bra       EVAL3

EVAL0               lsrb                          ; SET UP
                    cmpb      #3                  ; CHECK FOR UNARY + OR -
                    bne       EVAL1
                    jsr       UPSCLR
EVAL1               ldx       BUFPNT
EVAL1A              jsr       SKYCLS              ; GET NEXT CHAR
                    stx       BUFPNT
                    cmpb      #4                  ; CHECK FOR OPERATORS
                    bls       EVAL1Z
                    ldab      #5                  ; SET UP
EVAL1Z              aslb
                    stab      OFFREL+1            ; SET UP BRANCH
OFFREL              bra       *
                    bra       EVAL2               ; ERROR
                    bra       EVAL4               ; TERMINATOR
                    bra       EVAL8               ; LETTER
                    bra       EVAL7               ; NUMBER
                    bra       EVAL1C              ; RIGHT PAREN

                    psha                          ; SAVE
                    inx
                    bra       EVA0A               ; AGAIN

EVAL1C              tsx                           ; GET SP
                    dex                           ; ADJUST
                    ldab      DIMFLG
                    cpx       STKTOP              ; CHECK FOR EMPTY
                    beq       EVAL1E
                    pula
                    clrb
                    cmpa      #'('                ; CHECK FOR L PAREN ON STACK
                    beq       EVAL1C              ; IF SO, OK
EVAL1E              tstb      CHECK               ; FOR ALRIGHT
                    beq       EVAL2               ; IF NOT SET, ERROR
EVAL4               clra
                    ldab      STKCNT              ; GET STACK STKCNT
                    decb                          ; CHECK OP STACK
                    cmpb      AUXCNT
                    bne       EVAL2               ; IF NOT EMPTY, ERROR
                    tsx
                    dex                           ; ALIGN
                    cpx       STKTOP              ; CHECK OPERATOR STACK
                    beq       EVAL3A              ; IF NOT EMPTY� ERROR
EVAL2               ldaa      #$20                ; SET ERROR NUMBER
EVAL3               lds       STKTOP              ; GET SP
EVAL3A              ldx       BUFPNT              ; SET POINTER
                    rts

EVAL7               jsr       STAKUP              ; SHIFT OP STACK UP
                    ldx       BUFPNT
                    jsr       BCDCON              ; GET OPERAND
                    bra       EVAL12

EVAL8               ldaa      1,x                 ; GET NEXT CHAR
                    jsr       CLASS1              ; GO CLASSIFY
                    cmpb      #2                  ; CHECK FOR LETTER
                    bne       EVAL9               ; IF NOT, VARIABLE
                    ldaa      ,x                  ; GET CHAR BACK
                    stx       XSAVE               ; SET FOR ENTRY TO FIMDKEY
                    ldx       #FCTTBL
                    jsr       FNDKE2              ; GO CHECK FUNCTION
                    tsta                          ; CHECK SUCCESS
                    beq       EVAL4
                    jmp       RUNEX4              ; GO SERVICE

EVAL86              ldaa      #'?'                ; GET STGNUM OPERATOR
EVAL87              psha                          ; PUT ON STACK
                    ldx       XSAVE
                    jmp       EVA0A

EVAL85              ldaa      #'@'                ; GET ABS OPERATOR
                    bra       EVAL87

EVAL88              jsr       UPSCLR              ; MOVE STACK UP
                    jsr       RANDOM              ; COMPUTE RANDOM #
                    staa      NUMBER+2
EVAL89              ldx       XSAVE               ; RESTORE POINTER
                    bra       EVAL12

EVAL9               ldab      STKTOP
                    pshb
                    ldab      STKTOP+1
                    pshb
                    ldab      AUXCNT              ; GET COUNTER
                    pshb                          ; SAVE
                    ldab      DIMFLG              ; GET FLAG
                    pshb                          ; SAVE
                    jsr       FNDLB0              ; FIND VARIABLE STORAGE
                    pulb                          ; GET FLAG
                    stab      DIMFLG              ; RESTORE
                    pulb                          ; GET COUNTER
                    stab      AUXCNT              ; RESTORE
                    pulb
                    stab      STKTOP+1
                    pulb
                    stab      STKTOP
                    bsr       STAKUP
                    ldx       XTEMP
                    bsr       GETVAL              ; MOVE VALUE TO NUMBER
                    bra       EVA12A

EVA11C              ldx       BUFPNT              ; RESTORE POINTER
                    inx
EVAL12              stx       BUFPNT              ; SAVE POINTER
EVA12A              tsx
                    dex
                    cpx       STKTOP              ; CHECK OPERATOR STACK
                    beq       EVAL10              ; IF EMPTY, DON'T OPERATE
                    pula
                    psha                          ; PUT BACK
                    cmpa      #'('                ; CHECK FOR LEFT PAREM
                    beq       EVAL10              ; IF SO, DON'T OPERATE
                    jsr       CLASS1              ; GO CLASSYFY
                    pshb
                    lsrb                          ; SET UP ID
                    ldaa      STKCNT              ; GET COUNT
                    deca
                    cmpb      #4                  ; CHECK FOR ABS OR SON
                    beq       EVA12C              ; IF SO, GO AHEAD
                    cmpa      AUXCNT              ; OTHERWISE CHECK FOR 2 OPERANDS
                    beq       EVAL10              ; IF NOT, ABORT
EVA12C              cmpa      #9                  ; CHECK OVERFLOW
                    bls       EVA12D              ; OK
                    ldaa      #$24                ; SET ERROR
                    bra       EVAL19

EVA12D              pula                          ; GET CLASSIFICATION
                    pulb                          ; GET OPERATOR
                    suba      #6                  ; REMOVE BIAS
                    asla                          ; #2
                    staa      OPOFF+1             ; SET UP JMP
                    ldx       #OPTBL              ; POINT
OPOFF               ldx       ,x
                    jsr       ,x                  ; GO OPERATE
                    jsr       ZCHK                ; CHECK RESULT
                    bvc       EVA12A              ; IF NO OVFL, GO OPERATE AGAIN
EVAL18              ldaa      #$23                ; SET ERROR NUMBER
EVAL19              jmp       EVAL3

EVAL10              jmp       EVAL1

;*******************************************************************************

OPTBL               fdb       ADD
                    fdb       SUB
                    fdb       SIGNUM
                    fdb       ABSVAL
                    fdb       MULT
                    fdb       DIVIDE
                    fdb       EXPON

;*******************************************************************************
; GET VALUE - MOVE 3 BYTES POINTED TO BY X TO NUMBER

GETVAL              proc
                    ldaa      ,x                  ; GET VALUE
                    staa      NUMBER              ; STORE
                    ldaa      1,x
                    staa      NUMBER+1
                    ldaa      2,x
                    staa      NUMBER+2
                    rts

;*******************************************************************************
; STACKUP - ROLL OPERATIONAL STACK UPWARD

STAKUP              proc
                    ldx       #STKEND             ; POINT TO END
Loop@@              ldab      3,x
                    stab      ,x                  ; MOVE
                    inx
                    cpx       #NUMBER             ; SEE IF DONE
                    bne       Loop@@
                    inc       STKCNT
                    rts

;*******************************************************************************
; STACKDOWN - ROLL OPERATIONAL STACK DOWNWARD

STAKDN              proc
                    ldx       #AX-1               ; POINT TO STORE
STAKD1              ldab      ,x
                    stab      3,x
                    dex
                    cpx       #STKEND-1           ; SEE IF DONE
                    bne       STAKD1
                    dec       STKCNT
                    rts

;*******************************************************************************
; UNSIGNED ADD OF AX TO NUMBER

UADD                proc
                    clc                           ; ZERO THE CARRY
UADD1               ldx       #NUMBER+2           ; POINT TO STORE
_1@@                ldaa      ,x                  ; GET ADDEND
                    adca      3,x                 ; ADD IN AUGEND
                    daa
                    staa      ,x                  ; SAVE
                    dex
                    cpx       #NUMBER-1           ; SEE IF DONE
                    bne       _1@@
UADD22              pshb
                    ldab      #$02                ; SET FOR OVFL
                    bita      #$F0                ; ANDAGAIN
                    bne       _2@@
                    clrb                          ; RESET OFVL
_2@@                orab      OVFLBF
                    stab      OVFLBF              ; SET OVFL IF NECESSARY
                    tba
                    pulb
                    rts

;*******************************************************************************
; UNSIGNED SUBTRACT OF AX FROM NUMBER

USUB                proc
                    bsr       TENCOM              ; GO TEN'S COMPLEMENT
                    sec                           ; FIX UP
                    bra       UADD1               ; GO ADD

;*******************************************************************************
; UNSIGNED TEN'S COMPLEMENT OF AX (ALMOST)

TENCOM              proc
                    ldx       #AX+2               ; POINT TO AX
TENCO1              ldaa      #$99
                    suba      ,x                  ; SUBTRACT FROM 99
                    staa      ,x                  ; SAVE
                    dex
                    cpx       #AX-1
                    bne       TENCO1
                    anda      #$0F                ; RESET SIGN
                    staa      1,x                 ; STORE
                    rts

;*******************************************************************************
; CALCULATE RESULT SIGN

SETSIN              proc
                    clr       OVFLBF              ; CLEAR OVFL INDICATOR
;                   bra       SETSI0

;*******************************************************************************

SETSI0              proc
                    ldaa      AX                  ; GET SIGN
                    tab                           ; SAVE
                    andb      #$0F                ; RESET SIGN
                    stab      AX                  ; PUT BACK
                    staa      AXSIGN              ; SAVE SIGN
                    eora      NUMBER              ; FORM NEW SIGN
                    staa      SIGN                ; SAVE
;                   bra       ABSVAL

;*******************************************************************************

ABSVAL              proc
                    ldab      NUMBER              ; GET MS BYTE
                    andb      #$0F                ; RESET SIGN
                    stab      NUMBER              ; PUT BACK
                    tsta                          ; TEST NEW SIGN
                    rts

;*******************************************************************************
; SUBTRACT AX FROM NUMBER

SUB                 proc
                    ldaa      NUMBER              ; GET MS BYTE
                    eora      #$F0                ; CHANGE SIGN
                    staa      NUMBER              ; PUT BACK
;                   bra       ADD

;*******************************************************************************
; ADDAX TO NUMBER

ADD                 proc
                    bsr       RELAY
                    bsr       SETSIN              ; GO CALCULATE SIGN
                    bpl       ADD0                ; USE EITHER SIGN
                    bsr       USUB                ; OTHERWISE SUBTRACT
                    tap                           ; SET CCR
                    bvc       ADD1                ; CHECK OVERFLOW
                    com       AXSIGN              ; CHANGE FOR AX SMALLER
                    bra       ADD15

ADD0                bsr       UADD                ; GO ADD
                    bra       ADD2                ; GO FIX SIGN

ADD1                bsr       RELAY               ; COPY NUMBER TO AX
                    jsr       UPSCLR              ; RESTORE
                    bsr       USUB                ; GO NEGATE
ADD15               clr       OVFLBF
ADD2                ldaa      AXSIGN              ; GET OLD SIGN
;                   bra       FIXSIN

;*******************************************************************************
; SET THE SIGN ON THE RESULT

FIXSIN              proc
                    anda      #$F0                ; MASK
                    ldab      #$0F                ; SET MASK
                    andb      NUMBER              ; RESET SIGN
                    aba                           ; TACK ON SIGN
                    staa      NUMBER              ; PUT BACK
FIX2                rts

;*******************************************************************************
; MULTIPLY AC BY AX

MULT                proc
                    bsr       RELAY               ; MOVE STACK
                    bsr       SETSIN              ; GO CALC. SIGNS
;                   bra       MULT0

;*******************************************************************************

MULT0               proc
                    jsr       UPSCLR              ; MOVE STACK UP
                    ldab      #5                  ; SET COUNTER
_1@@                ldaa      AC                  ; GET MS BYTE OF AC
                    beq       _3@@                ; IF ZERO , LOOP
_2@@                jsr       UADD                ; ADD IN AX
                    dec       AC                  ; ONCE DONE
                    bne       _2@@
_3@@                decb                          ; ONCE DONE
                    beq       MULT4               ; CHECK IF ALL DONE
                    bsr       ACLEFT              ; SHIFT AC LEFT
                    ldaa      NUMBER
                    jsr       UADD22
                    bra       _1@@

MULT4               ldaa      SIGN                ; GET THE SIGN
                    bsr       FIXSIN              ; GO FIX UP THE SIGN
                    ldx       #AC-1               ; POINT TO AC
                    jmp       STAKD1              ; MOVE STACK BACK

;*******************************************************************************

RELAY               jmp       STAKDN              ; RELAY TO STACK DOWN

;*******************************************************************************
; DIVIDE AC-NUMBER BY AX

DIVIDE              proc
                    bsr       RELAY
                    ldx       #AX
                    jsr       ZCHK1               ; GO CHECK IF AX=O
                    bne       _1@@                ; IF NOT, OK
                    ldaa      #$22                ; SET ERROR
                    jmp       EVAL3

_1@@                jsr       SETSIN              ; CALC, SIGNS
                    jsr       STAKUP              ; PUSH BACK
                    bsr       ACLEFT              ; SHIFT DOWN
                    clr       2,x
                    clr       3,x                 ; ZERO OUT NUMBER
                    ldab      #5                  ; SET LOOP COUNT
_2@@                bsr       ACLEFT              ; MOVE AC DOWN
                    jsr       TENCOM              ; TAKE 10'S COMP
_3@@                bsr       DADD                ; GO SPECIAL ADD
                    bita      #$F0                ; CHECK FOR OVERFLOW
                    bne       _4@@
                    jsr       TENCOM              ; IF SO, RESTORE AX
                    clc
                    bsr       DADD1               ; ADDBACK IN
                    decb                          ; ONE PASS MADE
                    bne       _2@@
                    bra       MULT4
_4@@                inc       NUMBER+2            ; ADD ONE IN
                    bra       _3@@                ; GO DO AGAIN

;*******************************************************************************
; SHIFT AC-NUMBER LEFT 4 BITS

ACLEFT              proc
                    ldaa      #4                  ; SET FOR 4 BITS
Loop@@              ldx       #AX-1               ; POINT X
                    clc
_@@                 rol       ,x                  ; ROTATE
                    dex
                    cpx       #AC-1               ; CHECK IF DONE
                    bne       _@@
                    deca                          ; CHECK FOR DONE
                    bne       Loop@@
                    rts

;*******************************************************************************
; ADDAX TO A C

DADD                proc
                    sec
;                   bra       DADD1

;*******************************************************************************

DADD1               proc
                    ldx       #AC+2
                    ldaa      AC                  ; GET MS BYTE
                    anda      #$0F                ; RESET SIGN
                    staa      AC                  ; STORE BACK
Loop@@              ldaa      ,x                  ; GET ADDEND
                    adca      6,x                 ; ADD IN
                    daa
                    staa      ,x                  ; SAVE
                    dex
                    cpx       #AC-1               ; SEE IF DONE
                    bne       Loop@@
                    rts

;*******************************************************************************
; CALCULATE SIGNUM FUNCTION

SIGNUM              proc
                    bsr       ZCHK                ; GO CHECK = O
                    beq       Zero@@              ; IF SOY RESULT =0
                    ldab      NUMBER              ; OTHERWISE GET SIGN
SIGNU1              bsr       Zero@@              ; GO CLEAR
                    inc       NUMBER+2            ; MAKE = I
                    tba                           ; SET FOR FIXSIN
                    jmp       FIXSIN              ; GO SET THE SIGN
Zero@@              jmp       CLRNUM

;*******************************************************************************
; CALCULATE EXPONENTIATION
; ONLY POSITIVE EXPONENTS UP TO 99 ALLOWED

EXPON               proc
                    bsr       RELAY               ; MOVE OPERANDS DOWN
                    clrb
                    stab      OVFLBF              ; CLEAR OVER FLOW
                    ldaa      AX+2                ; GET EXPONENT
                    beq       SIGNU1              ; IF O, GO MAKE RESULT +1
                    jsr       STAKUP              ; GET TWO COPIES
                    jsr       RELAY               ; MOVE DOWN
Loop@@              adda      #$99                ; DECREMENT
                    daa
                    beq       Done@@              ; WHEN 0 ALL DONE
                    psha                          ; SAVE EXP
                    jsr       SETSI0              ; GO FIX SIGNS
                    jsr       MULT0               ; GO MULTIPLY
                    pula                          ; GET EXPONENT
                    bra       Loop@@              ; LOOP
Done@@              equ       :AnRTS

;*******************************************************************************
; FULL COMPARE ON X
; COMPARES X WITH CONTENTS OF CPX1

CMPX                proc
                    stx       CPX2                ; SAVE
;                   bra       CMPX1

;*******************************************************************************

CMPX1               proc
                    ldaa      CPX2                ; GET MS BYTE
                    cmpa      CPX1                ; COMPARE
                    bne       Done@@              ; IF NOT EQUAL, DONE
                    ldab      CPX2+1              ; GET LS BYTE
                    cmpb      CPX1+1              ; COMPARE
Done@@              rts                           ; DOME

;*******************************************************************************
; CHECK OPERAND FOR EQUAL TO 0

ZCHK                proc
                    ldx       #NUMBER
;                   bra       ZCHK1

;*******************************************************************************

ZCHK1               proc
                    clrb
                    tst       2,x
                    bne       _@@
                    tst       1,x
                    bne       _@@
                    ldaa      ,x                  ; GET MS BYTE
                    anda      #$0F
                    bne       _@@                 ; CHECK FOR 0
                    staa      ,x                  ; RESET SIGN BITS
                    ldab      #4
_@@                 ldaa      ,x                  ; GET MS BYTE
                    rora                          ; MOVE A SIGN BIT TO N
                    anda      #8                  ; MASK N BIT
                    aba                           ; MERGE Z AND N
                    oraa      OVFLBF              ; ADD IN V
                    tap                           ; SET CCR
                    rts

;*******************************************************************************

SKYCLS              proc
                    jsr       SKIPSP
                    bra       CLASS1

;*******************************************************************************
; CLASSIFY A CHARACTER IN THE A ACCUMULATOR
; CLASSIFICATION RETURNED IN B
;  0 ERROR
;  1 TERMINATOR
;  2 LETTER
;  3 NUMBER
;  4 )
;  5 (
;  6 +
;  7 -
;  8 SGN
;  9 ABS
; 10 *
; 11 /
; 12 ~

CLASS               proc
                    ldaa      ,x                  ; GET CHAR
;                   bra       CLASS1

;*******************************************************************************

CLASS1              proc
                    ldab      #1                  ; SET UP
                    cmpa      #$D                 ; CHECK FOR CR
                    beq       Done@@
                    decb
                    psha                          ; SAVE CHAR
                    suba      #'('                ; REMOVE BIAS
                    bmi       _2@@                ; CHECK ILLEGAL
                    cmpa      #'@'-'('            ; CHECK LIMIT
                    bls       _3@@                ; NOT LETTER
                    cmpa      #'Z'-'('            ; CHECK FOR LETTER
                    bls       _1@@
                    cmpa      #'^'-'('            ; CHECK FOR ILLEGAL
                    bne       _2@@
                    ldab      #10                 ; FIX UP
_1@@                addb      #02
_2@@                pula                          ; RESTORE CHARACTER
Done@@              rts                           ; DONE

_3@@                stx       XSAVE2              ; SAVE X REG
                    ldx       #Table@@            ; POINT TO TABLE
                    staa      CLSOFF+1            ; SET BIAS
CLSOFF              ldab      ,x                  ; GET CLASSIFICATION
                    ldx       XSAVE2              ; RESTORE X REG,
                    bra       _2@@

Table@@             fcb       5,4,10,6,1,7,0,11,3,3,3,3
                    fcb       3,3,3,3,3,3,1,1,1,1,1,8,9

;*******************************************************************************
; RANDOM GENERATOR

RANDOM              proc
                    ldab      #8                  ; SET COUNTER
                    ldx       #RNDM
RPT                 ldaa      3,x                 ; GET M.S. BYTE OF RANDOM NO.
                    asla:3                        ; LEFT 3 TIMES TO GET BIT 28 IN LINE WITH BIT 31
                    eora      3,x                 ; XOR A WITH RANDOM NO
                    asla:2                        ; PUT BIT 28.XOR31 IN CARRY BY SHIFTING LEFT
                    rol       ,x                  ; ROTATE ALL FOUR BYTES OF
                    rol       1,x                 ; THE RANDOM NO, ROTATING
                    rol       2,x                 ; THE CARRY INTO THE LSB
                    rol       3,x                 ; THE MSB IS LOST
                    decb                          ; DECREMENT THE COUNTER
                    bne       RPT                 ; IF ITS NOT O, GO REPEAT
                    ldaa      ,x                  ; PUT RANDOM # IN A
                    cmpa      #$9F                ; CHECK IN RANGE
                    bhi       RANDOM              ; IN NOT GET ANOTHER
                    adda      #0                  ; SET HALF CARRY
                    daa
                    rts

ENDSTR              rmb       2
STORSP              equ       *

                    org       EXTERN
                    rts
