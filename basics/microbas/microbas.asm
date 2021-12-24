; NAM MICRO  MICROBASIC

; ***** VERSION 1.3A *****
;
; BY ROBERT H UITERWYK, TAMPA, FLORIDA
;
; *THIS PROGRAM ASSUMES THAT THE
; *MOTOROLA MIKBUG ROM IS INSTALLED
; *AND THAT ITS ASSOCIATED 128 BYTE
; *RAN IS ALSO PRESENT
; *THE SP AND XSTACK WILL HAVE TO
; *BE MOVED IF THIS IS NOT THE CASE
;
; Modified to run on Corsham Tech 6800 by
; Christopher "Zoggins" Mallery
;

                    org       $20
INDEX1              fdb       $0000
INDEX2              fdb       $0000
INDEX3              fdb       $0000
INDEX4              fdb       $0000
SAVESP              fdb       $0000
NEXTBA              fdb       END
WORKBA              fdb       END
SOURCE              fdb       END
PACKLN              fdb       $0000
HIGHLN              fdb       $0000
BASPNT              fdb       $0000
BASLIN              fdb       $0000
PUSHTX              fdb       $0000
XSTACK              fdb       $A07F
RNDVAL              fdb       $0000
DIMPNT              fdb       $0000
DIMCAL              fdb       $0000
PRCNT               fcb       0
MAXLIN              fcb       72
BACKSP              fcb       $0F
CANCEL              fcb       $18
MEMEND              fdb       $1FFF
ARRTAB              fdb       $0000
KEYWD               fdb       $0000
TSIGN               fcb       0
NCMPR               fcb       0
TNUMB               fcb       0
ANUMB               fcb       0
BNUMB               fcb       0
AESTK               fdb       ASTACK
FORPNT              fdb       FORSTK
VARPNT              fdb       VARTAB
SBRPNT              fdb       SBRSTK
SBRSTK              rmb       16
FORSTK              rmb       48
DIMVAR              fdb       VARTAB

                    org       $00AC
BUFNXT              fdb       $00B0
ENDBUF              fdb       $00B0
                    org       $00B0
BUFFER              rmb       $50

                    org       $0100
PROGM               jmp       START

VARTAB              rmb       78
                    fcb       $1E
COMMAN              fcc       'RUN'
                    fcb       $1E
                    fdb       RUN
                    fcc       'LIST'
                    fcb       $1E
                    fdb       CLIST
                    fcc       'NEW'
                    fcb       $1E
                    fdb       START
                    fcc       'PAT'
                    fcb       $1E
                    fdb       PATCH
GOLIST              fcc       'GOSUB'
                    fcb       $1E
                    fdb       GOSUB
                    fcc       'GOTO'
                    fcb       $1E
                    fdb       GOTO
                    fcc       'GO TO'
                    fcb       $1E
                    fdb       GOTO
                    fcc       'SIZE'
                    fcb       $1E
                    fdb       SIZE
                    fcc       'THEN'
                    fcb       $1E
                    fdb       IF2
                    fcc       'PRINT'
                    fcb       $1E
                    fdb       PRINT
                    fcc       'LET'
                    fcb       $1E
                    fdb       LET
                    fcc       'INPUT'
                    fcb       $1E
                    fdb       INPUT
                    fcc       'IF'
                    fcb       $1E
                    fdb       IF
                    fcc       'END'
                    fcb       $1E
                    fdb       READY
                    fcc       'RETURN'
                    fcb       $1E
                    fdb       RETURN
                    fcc       'DIM'
                    fcb       $1E
                    fdb       DIM
                    fcc       'FOR'
                    fcb       $1E
                    fdb       FOR
                    fcc       'NEXT'
                    fcb       $1E
                    fdb       NEXT
                    fcc       'EEM'
                    fcb       $1E
                    fdb       REMARK
PAUMSG              fcc       'PAUSE'
                    fcb       $1E
                    fdb       PAUSE
                    fcb       $20
COMEND              fcb       $1E
IMPLET              fdb       LET
                    rmb       60
ASTACK              equ       *-1
RDYMSG              fcb       $0D
                    fcb       $0A
                    fcb       $15
                    fcb       $0A
                    fcb       $15
                    fcc       'READY'
                    fcb       $1E
PROMPT              fcb       $23
                    fcb       $00
                    fcb       $1E
                    fcb       $1E
PGCNTL              fcb       $10
                    fcb       $16
                    fcb       $1E
                    fcb       $1E
                    fcb       $1E
ERRMS1              fcc       'ERROR# '
                    fcb       $1E
ERRMS2              fcc       ' IN LINE '
                    fcb       $1E
KEYBD               ldaa      #$3F
                    bsr       OUTCH
KEYBD0              ldx       #BUFFER
                    ldab      #10
KEYBD1              bsr       INCH
                    cmpa      #$00
                    bne       KEYB11
                    decb
                    bne       KEYBD1
KEYB10              jmp       READY

KEYB11              cmpa      CANCEL
                    beq       DEL
                    cmpa      #$0D
                    beq       IEXIT
KEYBD2              cmpa      #$0A
                    beq       KEYBD1
                    cmpa      #$15
                    beq       KEYBD1
                    cmpa      #$13
                    beq       KEYBD1
KEYB55              cmpa      BACKSP
                    bne       KEYBD3
                    cpx       #BUFFER
                    beq       KEYBD1
                    dex
                    bra       KEYBD1

KEYBD3              cpx       #BUFFER+71
                    beq       KEYBD1
                    staa      0,X
                    inx
                    bra       KEYBD1

DEL                 bsr       CRLF
CNTLIN              ldx       #PROMPT
                    bsr       OUTNCR
                    bra       KEYBD0

IEXIT               ldaa      #$1E
                    staa      0,X
                    stx       ENDBUF
                    bsr       CRLF
                    rts

OUTCH               bsr       BREAK
                    jmp       OUTEEE

OUTEEE              equ       $E1D1

INCH                jmp       INEEE

BREAK               jmp       CBREAK1             ; #CJM: Calling my BREAK function, was BREAK1

;****************************************
;**** Original BREAK function for
;**** SWTPC 6800 w/ MP-C board at $8004
;****
BREAK1              psha
                    ldaa      PIAD
PIAD                equ       $8004
                    bmi       BREAK2
                    jmp       READY

BREAK2              pula
                    rts

;***************************************

INEEE               equ       $E1AC
OUTPUT              equ       *
                    bsr       OUTNCR
                    bra       CRLF

OUTPU2              bsr       OUTCH
OUTPU3              inx
OUTNCR              ldaa      0,X
                    cmpa      #$1E
                    bne       OUTPU2
                    rts

CRLF                bsr       PUSHX
                    ldx       #CRLFST
                    bsr       OUTNCR
                    bsr       PULLX
                    rts

CRLFST              fcb       $00
                    fcb       $0D
                    fcb       $0A
                    fcb       $15
CREND               fcb       $1E
                    fcb       $FF,$FF
                    fcb       $FF,$FF
                    fcb       $1E
PUSHX               stx       PUSHTX
                    ldx       XSTACK
                    dex
                    dex
                    stx       XSTACK
                    psha
                    ldaa      PUSHTX
                    staa      0,X
                    ldaa      PUSHTX+1
                    staa      1,X
                    pula
                    ldx       PUSHTX
                    rts

PULLX               ldx       XSTACK
                    ldx       0,X
                    inc       XSTACK+1
                    inc       XSTACK+1
                    rts

STORE               psha
                    pshb
                    bsr       PUSHX
                    jsr       PULLAE
                    ldx       AESTK
                    inx
                    inx
                    stx       AESTK
                    dex
                    ldx       0,X
                    staa      0,X
                    stab      1,X
                    bsr       PULLX
                    pulb
                    pula
                    rts

IND                 bsr       PUSHX
                    psha
                    pshb
                    ldx       AESTK
                    inx
                    inx
                    stx       AESTK
                    dex
                    ldx       0,X
                    ldaa      0,X
                    ldab      1,X
                    jsr       PUSHAE
                    pulb
                    pula
                    bsr       PULLX
                    rts

LIST                ldx       NEXTBA
                    stx       WORKBA
                    ldx       SOURCE
                    bra       LIST1

LIST0               ldx       INDEX3
LIST1               cpx       WORKBA
                    beq       LEXIT
                    bsr       OUTLIN
                    inx
                    bra       LIST1

LEXIT               rts

OUTLIN              ldaa      0,X
                    clr       PRCNT
                    inx
                    ldab      0,X
                    inx
                    clr       TSIGN
                    jsr       PRN0
                    bsr       PRINSP
OUTLI1              ldaa      0,X
                    inx
                    bsr       PUSHX
                    ldx       #COMMAN
                    stx       KEYWD
                    staa      KEYWD+1
                    ldx       KEYWD
                    dex
OUTLI2              dex
                    ldaa      0,X
                    cmpa      #$1E
                    bne       OUTLI2
                    inx
                    inx
                    inx
                    jsr       OUTNCR
                    bsr       PULLX
                    jmp       OUTPUT

PRINSP              psha
                    ldaa      #$20
                    jsr       OUTCH
                    pula
                    rts

RANDOM              inx
                    inx
                    ldaa      0,X
                    cmpa      #'D'
                    bne       TSTVER
                    jsr       PUSHX
                    ldaa      RNDVAL
                    ldab      RNDVAL+1
                    ldx       #0000
RAND1               adcb      1,X
                    adca      0,X
                    inx
                    inx
                    cpx       #RNDVAL
                    bne       RAND1
                    anda      #$7F
                    staa      RNDVAL
                    stab      RNDVAL+1
                    stx       INDEX1
                    ldaa      INDEX1
                    ldab      INDEX1+1
                    jmp       TSTV9

TSTV                jsr       SKIPSP
                    jsr       BREAK
                    jsr       TSTLTR
                    bcc       TSTV1
                    rts

TSTV1               cmpa      #'R'
                    bne       TSTV2
                    ldab      1,X
                    cmpb      #'N'
                    beq       RANDOM
TSTV2               jsr       PUSHX
                    suba      #$40
                    staa      VARPNT+1
                    asla
                    adda      VARPNT+1
                    staa      VARPNT+1
                    ldx       VARPNT
                    ldaa      VARPNT
                    ldab      VARPNT+1
                    tst       2,X
                    bne       TSTV20
                    bra       TSTV9

TSTV20              ldx       0,X
                    stx       DIMPNT
                    inx
                    inx
                    stx       DIMCAL
                    jsr       PULLX
                    jsr       INXSKP
                    cmpa      #'('
                    beq       TSTV22
TSTVER              jmp       DBLLTR

TSTV22              inx
                    jsr       EXPR
                    jsr       PUSHX
                    jsr       PULLAE
                    tsta
                    beq       TSTV3
SUBER1              jmp       SUBERR

TSTV3               ldx       DIMPNT
                    tstb
                    beq       SUBER1
                    cmpb      0,X
                    bhi       SUBER1
                    ldaa      1,X
                    staa      ANUMB
                    beq       TST666
                    ldx       DIMCAL
TSTV4               decb
                    beq       TSTV6
                    ldaa      ANUMB
TSTV5               inx
                    inx
                    deca
                    bne       TSTV5
                    bra       TSTV4

TSTV6               stx       DIMCAL
                    jsr       PULLX
                    jsr       SKIPSP
                    cmpa      #','
                    bne       TSTVER
                    inx
                    jsr       EXPR
                    jsr       PUSHX
                    bsr       PULLAE
                    tsta
                    bne       SUBER1
                    ldx       DIMPNT
                    tstb
                    beq       SUBER1
                    cmpb      1,X
                    bhi       SUBER1
TST666              ldx       DIMCAL
TSTV7               inx
                    inx
                    decb
                    bne       TSTV7
                    dex
                    dex
                    stx       DIMCAL
                    jsr       PULLX
                    jsr       SKIPSP
TSTV8               cmpa      #')'
                    bne       TSTVER
                    jsr       PUSHX
                    ldaa      DIMCAL
                    ldab      DIMCAL+1
TSTV9               jsr       PULLX
                    inx
                    bsr       PUSHAE
                    clc
                    rts

TSTLTR              cmpa      #$41
                    bmi       NONO
                    cmpa      #$5A
                    ble       YESNO
TESTNO              cmpa      #$30
                    bmi       NONO
                    cmpa      #$39
                    ble       YESNO
NONO                sec
                    rts

YESNO               clc
                    rts

PULPSH              bsr       PULLAE
PUSHAE              sts       SAVESP
                    lds       AESTK
                    pshb
                    psha
                    sts       AESTK
                    lds       SAVESP
                    rts

PULLAE              sts       SAVESP
                    lds       AESTK
                    pula
                    pulb
                    sts       AESTK
                    lds       SAVESP
                    rts

FACT                jsr       SKIPSP
                    jsr       TSTV
                    bcs       FACT0
                    jsr       IND
                    rts

FACT0               jsr       TSTN
                    bcs       FACT1
                    rts

FACT1               cmpa      #'('
                    bne       FACT2
                    inx
                    bsr       EXPR
                    jsr       SKIPSP
                    cmpa      #')'
                    bne       FACT2
                    inx
                    rts

FACT2               ldab      #13
                    jmp       ERROR

TERM                bsr       FACT
TERM0               jsr       SKIPSP
                    cmpa      #'*'
                    bne       TERM1
                    inx
                    bsr       FACT
                    bsr       MPY
                    bra       TERM0

TERM1               cmpa      #'/'
                    bne       TERM2
                    inx
                    bsr       FACT
                    jsr       DIV
                    bra       TERM0

TERM2               rts

EXPR                jsr       SKIPSP
                    cmpa      #'-'
                    bne       EXPR0
                    inx
                    bsr       TERM
                    jsr       NEG
                    bra       EXPR1

EXPR0               cmpa      #'+'
                    bne       EXPR00
                    inx
EXPR00              bsr       TERM
EXPR1               jsr       SKIPSP
                    cmpa      #'+'
                    bne       EXPR2
                    inx
                    bsr       TERM
                    jsr       ADD
                    bra       EXPR1

EXPR2               cmpa      #'-'
                    bne       EXPR3
                    inx
                    bsr       TERM
                    jsr       SUB
                    bra       EXPR1

EXPR3               rts

MPY                 bsr       MDSIGN
                    ldaa      #15
                    staa      0,X
                    clrb
                    clra
MPY4                lsr       3,X
                    ror       4,X
                    bcc       MPY5
                    addb      2,X
                    adca      1,X
                    bcc       MPY5
MPYERR              ldaa      #2
                    jmp       ERROR

MPY5                asl       2,X
                    rol       1,X
                    dec       0,X
                    bne       MPY4
                    tsta
                    bmi       MPYERR
                    tst       TSIGN
                    bpl       MPY6
                    bsr       NEGAB
MPY6                stab      4,X
                    staa      3,X
                    jsr       PULLX
                    rts

MDSIGN              jsr       PUSHX
                    clra
                    ldx       AESTK
                    tst       1,X
                    bpl       MDS2
                    bsr       NEG
                    ldaa      #$80
MDS2                inx
                    inx
                    stx       AESTK
                    tst       1,X
                    bpl       MDS3
                    bsr       NEG
                    adda      #$80
MDS3                staa      TSIGN
                    dex
                    dex
                    rts

DIV                 bsr       MDSIGN
                    tst       1,X
                    bne       DIV33
                    tst       2,X
                    bne       DIV33
                    ldab      #8
                    jmp       ERROR

DIV33               ldaa      #1
DIV4                inca
                    asl       2,X
                    rol       1,X
                    bmi       DIV5
                    cmpa      #17
                    bne       DIV4
DIV5                staa      0,X
                    ldaa      3,X
                    ldab      4,X
                    clr       3,X
                    clr       4,X
DIV163              subb      2,X
                    sbca      1,X
                    bcc       DIV165
                    addb      2,X
                    adca      1,X
                    clc
                    bra       DIV167

DIV165              sec
DIV167              rol       4,X
                    rol       3,X
                    lsr       1,X
                    ror       2,X
                    dec       0,X
                    bne       DIV163
                    tst       TSIGN
                    bpl       DIV169
                    bsr       NEG
DIV169              jsr       PULLX
                    rts

NEG                 psha
                    pshb
                    jsr       PULLAE
                    bsr       NEGAB
                    jsr       PUSHAE
                    pulb
                    pula
                    rts

NEGAB               coma
                    comb
                    addb      #1
                    adca      #0
                    rts

SUB                 bsr       NEG
ADD                 jsr       PULLAE
ADD1                stab      BNUMB
                    staa      ANUMB
                    jsr       PULLAE
                    addb      BNUMB
                    adca      ANUMB
                    jsr       PUSHAE
                    clc
                    rts

FINDNO              ldaa      HIGHLN
                    ldab      HIGHLN+1
                    subb      PACKLN+1
                    sbca      PACKLN
                    bcs       HIBALL
FINDN1              ldx       SOURCE
FIND0               jsr       PULPSH
                    subb      1,X
                    sbca      0,X
                    bcs       FIND3
                    bne       FIND1
                    tstb
                    beq       FIND4
FIND1               inx
FIND2               bsr       INXSKP
                    cmpa      #$1E
                    bne       FIND2
                    inx
                    cpx       NEXTBA
                    bne       FIND0
HIBALL              ldx       NEXTBA
FIND3               sec
FIND4               stx       WORKBA
                    jsr       PULLAE
                    rts

SKIPSP              ldaa      0,X
                    cmpa      #$20
                    bne       SKIPEX
INXSKP              inx
                    bra       SKIPSP

SKIPEX              rts

LINENO              jsr       INTSTN
                    bcc       LINE1
                    ldab      #7
                    jmp       ERROR

LINE1               jsr       PULPSH
                    staa      PACKLN
                    stab      PACKLN+1
                    stx       BUFNXT
                    rts

NXTLIN              ldx       BASPNT
NXTL12              ldaa      0,X
                    inx
                    cmpa      #$1E
                    bne       NXTL12
                    stx       BASLIN
                    rts

CCODE               bsr       SKIPSP
                    stx       INDEX4
                    sts       SAVESP
                    ldx       #COMMAN-1
LOOP3               lds       INDEX4
                    des
LOOP4               inx
                    pula
                    ldab      0,X
                    cmpb      #$1E
                    beq       LOOP7
                    cba
                    beq       LOOP4
LOOP5               inx
                    cpx       #COMEND
                    beq       CCEXIT
                    ldab      0,X
                    cmpb      #$1E
                    bne       LOOP5
LOOP6               inx
                    inx
                    bra       LOOP3

LOOP7               inx
                    sts       BUFNXT
                    sts       BASPNT
LOOP8               lds       SAVESP
                    rts

CCEXIT              lds       SAVESP
                    ldx       #IMPLET
                    rts

START               ldx       SOURCE
                    stx       NEXTBA
                    stx       WORKBA
                    stx       ARRTAB
                    dex
                    clra
START2              inx
                    staa      0,X
                    cpx       MEMEND
                    bne       START2
START1              clra
                    staa      PACKLN
                    staa      PACKLN+1
                    staa      PRCNT
                    ldx       PACKLN
                    stx       HIGHLN
READY               lds       #$A045
                    ldx       #RDYMSG
                    jsr       OUTPUT
NEWLIN              lds       #$A045
                    ldx       #$A07F
                    stx       XSTACK
                    clr       PRCNT
NEWL3               jsr       CNTLIN
                    ldx       #BUFFER
                    jsr       SKIPSP
                    stx       BUFNXT
                    jsr       TESTNO
                    bcs       LOOP2
                    jmp       NUMBER

LOOP2               cmpa      #$1E
                    beq       NEWLIN
                    bsr       CCODE
                    ldx       0,X
                    jmp       0,X

ERROR               lds       #$A045
                    jsr       CRLF
                    ldx       #ERRMS1
                    jsr       OUTNCR
                    clra
                    jsr       PUSHAE
                    jsr       PRN
                    ldx       #ERRMS2
                    jsr       OUTNCR
                    clrb
                    ldaa      BASLIN
                    beq       ERROR2
                    ldx       BASLIN
                    ldaa      0,X
                    ldab      1,X
ERROR2              jsr       PRN0
                    jsr       CRLF
                    bra       READY

RUN                 ldx       SOURCE
                    stx       BASLIN
                    ldx       #SBRSTK
                    stx       SBRPNT
                    ldx       #FORSTK
                    stx       FORPNT
                    ldx       #$A07F
                    stx       XSTACK
                    ldx       NEXTBA
                    stx       ARRTAB
                    clra
                    dex
RUN1                inx
                    staa      0,X
                    cpx       MEMEND
                    bne       RUN1
                    ldx       #VARTAB
                    ldab      #78
RUN2                staa      0,X
                    inx
                    decb
                    bne       RUN2
                    jmp       BASIC

CLIST               ldx       #PGCNTL
                    jsr       OUTPUT
                    ldx       BASPNT
CLIST1              jsr       SKIPSP
                    cmpa      #$1E
                    beq       CLIST4
                    jsr       INTSTN
                    stx       BASPNT
                    jsr       FINDN1
                    stx       INDEX3
                    ldx       BASPNT
                    psha
                    jsr       SKIPSP
                    cmpa      #$1E
                    pula
                    bne       CLIST2
                    jsr       PUSHAE
                    bra       CLIST3

CLIST2              inx
                    jsr       INTSTN
CLIST3              clra
                    ldab      #1
                    jsr       ADD1
                    jsr       FINDN1
                    jsr       LIST0
                    bra       CLIST5

CLIST4              jsr       LIST
CLIST5              jmp       REMARK

                    nop

PATCH               jsr       NXTLIN
                    ldx       #BASIC
                    stx       $A046
                    lds       #$A040
                    sts       SP
SP                  equ       $A008
                    jmp       CONTRL

CONTRL              equ       $E0E3

NUMBER              jsr       LINENO
NUM1                jsr       FINDNO
                    bcc       DELREP
                    ldx       WORKBA
                    cpx       NEXTBA
                    beq       CAPPEN
                    bsr       INSERT
                    bra       NEXIT

DELREP              ldx       BUFNXT
                    jsr       SKIPSP
                    cmpa      #$1E
                    bne       REPLAC
                    ldx       NEXTBA
                    cpx       SOURCE
                    beq       NEXIT
                    bsr       DELETE
                    bra       NEXIT

REPLAC              bsr       DELETE
                    bsr       INSERT
NEXIT               jmp       NEWLIN

CAPPEN              bsr       INSERT
                    ldx       PACKLN
                    stx       HIGHLN
                    bra       NEXIT

DELETE              sts       SAVESP
                    ldx       WORKBA
                    lds       NEXTBA
                    ldab      #2
                    inx
                    inx
                    des
                    des
DEL2                ldaa      0,X
                    des
                    inx
                    incb
                    cmpa      #$1E
                    bne       DEL2
                    sts       NEXTBA
                    sts       ARRTAB
                    ldx       WORKBA
                    stab      DEL5+1
; IN AT OBJECT TIME
DEL4                cpx       NEXTBA
                    beq       DELEX
DEL5                ldaa      0,X
                    staa      0,X
                    inx
                    bra       DEL4

DELEX               lds       SAVESP
                    rts

INSERT              ldx       BUFNXT
                    jsr       CCODE
INS1                stx       KEYWD
                    ldab      ENDBUF+1
                    subb      BUFNXT+1
                    addb      #$04
                    stab      OFFSET+1
                    addb      NEXTBA+1
                    ldaa      #$00
                    adca      NEXTBA
                    cmpa      MEMEND
                    bhi       OVERFL
                    stab      NEXTBA+1
                    staa      NEXTBA
                    ldx       NEXTBA
                    stx       ARRTAB
INS2                cpx       WORKBA
                    beq       BUFWRT
                    dex
                    ldaa      0,X
OFFSET              staa      0,X
                    bra       INS2

BUFWRT              ldx       WORKBA
                    sts       SAVESP
                    ldaa      PACKLN
                    staa      0,X
                    inx
                    ldaa      PACKLN+1
                    staa      0,X
                    inx
                    ldaa      KEYWD+1
                    staa      0,X
                    inx
                    lds       BUFNXT
                    des
BUF3                pula
                    staa      0,X
                    inx
                    cmpa      #$1E
                    bne       BUF3
                    lds       SAVESP
                    rts

OVERFL              ldab      #14
                    jmp       ERROR

BASIC               ldx       BASLIN
                    cpx       NEXTBA
                    bne       BASIC1
BASIC0              jmp       READY

BASIC1              tstb      ASLIN
                    beq       BASIC0
                    inx
                    inx
                    ldaa      0,X
                    inx
                    stx       BASPNT
                    ldx       #COMMAN
                    stx       KEYWD
                    staa      KEYWD+1
                    ldx       #ASTACK
                    stx       AESTK
                    ldx       KEYWD
                    ldx       0,X
BASIC2              jmp       0,X

GOSUB               ldx       BASLIN
                    stx       INDEX1
                    jsr       NXTLIN
                    ldx       SBRPNT
                    cpx       #SBRSTK+16
                    bne       GOSUB1
                    ldab      #9
                    jmp       ERROR

GOSUB1              ldaa      BASLIN
                    staa      0,X
                    inx
                    ldaa      BASLIN+1
                    staa      0,X
                    inx
                    stx       SBRPNT
                    ldx       INDEX1
                    stx       BASLIN
GOTO                ldx       BASPNT
                    jsr       EXPR
                    jsr       FINDN1
                    bcc       GOTO2
                    ldab      #7
                    jmp       ERROR

GOTO2               stx       BASLIN
                    bra       BASIC

RETURN              ldx       SBRPNT
                    cpx       #SBRSTK
                    bne       RETUR1
                    ldab      #10
                    jmp       ERROR

RETUR1              dex
                    dex
                    stx       SBRPNT
                    ldx       0,X
                    stx       BASLIN
                    bra       BASIC

PAUSE               ldx       #PAUMSG
                    jsr       OUTNCR
                    jsr       PRINSP
                    ldx       BASLIN
                    ldaa      0,X
                    inx
                    ldab      0,X
                    inx
                    jsr       PRN0
PAUSE1              jsr       INCH
                    cmpa      #$0D
                    bne       PAUSE1
                    jsr       CRLF
PAUSE2              jmp       REMARK

INPUT               ldaa      BASPNT
                    bne       INPUT0
                    ldab      #12
                    bra       INPERR

INPUT0              jsr       KEYBD
                    ldx       #BUFFER
                    stx       BUFNXT
                    ldx       BASPNT
INPUT1              jsr       TSTV
                    bcs       INPEX
                    stx       BASPNT
                    ldx       BUFNXT
INPUT2              bsr       INNUM
                    bcc       INPUT4
                    dex
                    ldaa      0,X
                    cmpa      #$1E
                    beq       INPUTS
                    ldab      #2
INPERR              jmp       ERROR

INPUTS              jsr       KEYBD
                    ldx       #BUFFER
                    bra       INPUT2

INPUT4              jsr       STORE
                    inx
                    stx       BUFNXT
                    ldx       BASPNT
                    jsr       SKIPSP
                    inx
                    cmpa      #','
                    beq       INPUT1
INPEX               dex
                    clr       PRCNT
                    cmpa      #$1E
                    beq       PAUSE2
DBLLTR              ldab      #3
                    jmp       ERROR

TSTN                bsr       INTSTN
                    bcs       TSTN0
                    jsr       PULLAE
                    tsta
                    bpl       TSTN1
TSTN0               sec
                    rts

TSTN1               jsr       PUSHAE
                    rts

INNUM               jsr       SKIPSP
                    staa      TSIGN
                    inx
                    cmpa      #'-'
                    beq       INNUM0
                    dex
INTSTN              clr       TSIGN
INNUM0              jsr       SKIPSP
                    jsr       TESTNO
                    bcc       INNUM1
                    rts

INNUM1              dex
                    clra
                    clrb
INNUM2              inx
                    psha
                    ldaa      0,X
                    jsr       TESTNO
                    bcs       INNEX
                    suba      #$30
                    staa      TNUMB
                    pula
                    aslb
                    rola
                    bcs       INNERR
                    stab      BNUMB
                    staa      ANUMB
                    aslb
                    rola
                    bcs       INNERR
                    aslb
                    rola
                    bcs       INNERR
                    addb      BNUMB
                    adca      ANUMB
                    bcs       INNERR
                    addb      TNUMB
                    adca      #0
                    bcc       INNUM2
INNERR              ldab      #2
                    jmp       ERROR

INNEX               pula
                    tst       TSIGN
                    beq       INNEX2
                    jsr       NEGAB
INNEX2              jsr       PUSHAE
                    clc
                    rts

PRINT               ldx       BASPNT
PRINT0              jsr       SKIPSP
                    cmpa      #'"'
                    bne       PRINT4
                    inx
PRINT1              ldaa      0,X
                    inx
                    cmpa      #'"'
                    beq       PRIN88
                    cmpa      #$1E
                    bne       PRINT2
                    ldab      #4
                    bra       PRINTE

PRINT2              jsr       OUTCH
                    bsr       ENLINE
                    bra       PRINT1

PRINT4              cmpa      #$1E
                    bne       PRINT6
                    dex
                    ldaa      0,X
                    inx
                    cmpa      #';'
                    beq       PRINT5
                    jsr       CRLF
                    clr       PRCNT
PRINT5              inx
                    stx       BASLIN
                    jmp       BASIC

PRINT6              cmpa      #'T'
                    bne       PRINT8
                    ldab      1,X
                    cmpb      #'A'
                    bne       PRINT8
                    inx
                    inx
                    ldaa      0,X
                    cmpa      #'B'
                    beq       PRINT7
                    ldab      #11
PRINTE              jmp       ERROR

PRINT7              inx
                    jsr       EXPR
                    jsr       PULLAE
                    subb      PRCNT
                    bls       PRIN88
PRIN77              jsr       PRINSP
                    bsr       ENLINE
                    decb
                    bne       PRIN77
                    bra       PRIN88

PRINT8              jsr       EXPR
                    bsr       PRN
PRIN88              jsr       SKIPSP
                    cmpa      #','
                    bne       PRIN99
                    inx
PRLOOP              ldaa      PRCNT
                    tab
                    andb      #$F8
                    sba
                    beq       PRI999
                    jsr       PRINSP
                    bsr       ENLINE
                    bra       PRLOOP

PRIN99              cmpa      #';'
                    bne       PREND
                    inx
PRI999              jmp       PRINT0

PREND               cmpa      #$1E
                    beq       PRINT4
                    ldab      #6
                    bra       PRINTE

ENLINE              psha
                    ldaa      PRCNT
                    inca
                    cmpa      MAXLIN
                    bne       ENLEXT
                    jsr       CRLF
                    clra
ENLEXT              staa      PRCNT
                    pula
                    rts

PRN                 jsr       PRINSP
                    bsr       ENLINE
                    ldaa      #$FF
                    staa      TSIGN
                    jsr       PULLAE
                    tsta
                    bpl       PRN0
                    jsr       NEGAB
                    psha
                    ldaa      #'-'
                    jsr       OUTCH
                    bsr       ENLINE
                    pula
PRN0                jsr       PUSHX
                    ldx       #KIOK
PRN1                clr       TNUMB
PRN2                subb      1,X
                    sbca      0,X
                    bcs       PRN5
                    inc       TNUMB
                    bra       PRN2

PRN5                addb      1,X
                    adca      0,X
                    psha
                    ldaa      TNUMB
                    bne       PRN6
                    cpx       #KIOK+8
                    beq       PRN6
                    tst       TSIGN
                    bne       PRN7
PRN6                adda      #$30
                    clr       TSIGN
                    jsr       OUTCH
                    bsr       ENLINE
PRN7                pula
                    inx
                    inx
                    cpx       #KIOK+10
                    bne       PRN1
                    jsr       PULLX
                    rts

KIOK                fdb       10000
                    fdb       1000
                    fdb       100
                    fdb       10
                    fdb       1

LET                 ldx       BASPNT
                    jsr       TSTV
                    bcc       LET1
LET0                ldab      #12
LET00               jmp       ERROR

LET1                jsr       SKIPSP
                    inx
                    cmpa      #'='
                    beq       LET3
LET2                ldab      #6
                    bra       LET00

LET3                jsr       EXPR
                    cmpa      #$1E
                    bne       LET2
                    jsr       STORE
                    bra       REMARK

SIZE                ldab      ARRTAB+1
                    ldaa      ARRTAB
                    subb      SOURCE+1
                    sbca      SOURCE
                    bsr       PRN0
                    jsr       PRINSP
                    ldab      MEMEND+1
                    ldaa      MEMEND
                    subb      ARRTAB+1
                    sbca      ARRTAB
                    jsr       PRN0
                    jsr       CRLF
REMARK              jsr       NXTLIN
                    jmp       BASIC

DIM                 ldx       BASPNT
DIM1                jsr       SKIPSP
                    jsr       TSTLTR
                    bcc       DIM111
                    jmp       DIMEX

DIM111              suba      #$40
                    staa      DIMVAR+1
                    asla
                    adda      DIMVAR+1
                    staa      DIMVAR+1
                    jsr       PUSHX
                    ldx       DIMVAR
                    tst       0,X
                    bne       DIMERR
                    tst       1,X
                    bne       DIMERR
                    tst       2,X
                    bne       DIMERR
                    ldaa      ARRTAB+1
                    staa      1,X
                    ldaa      ARRTAB
                    staa      0,X
                    staa      2,X
                    jsr       PULLX
                    jsr       INXSKP
                    cmpa      #'('
                    beq       DIM2
DIMERR              ldab      #5
DIMER1              jmp       ERROR

DIM2                inx
                    jsr       EXPR
                    jsr       PULPSH
                    tstb
                    beq       SUBERR
                    tsta
                    beq       DIM3
SUBERR              ldab      #15
                    bra       DIMER1

DIM3                bsr       STRSUB
                    ldaa      0,X
                    cmpa      #','
                    bne       DIM6
                    inx
                    jsr       EXPR
                    jsr       PULPSH
                    tstb
                    beq       SUBERR
                    tsta
                    bne       SUBERR
                    bsr       STRSUB
                    jsr       MPY
DIM6                clra
                    ldab      #2
                    jsr       PUSHAE
                    jsr       MPY
                    ldaa      0,X
                    cmpa      #')'
                    bne       DIMERR
                    inx
                    ldab      ARRTAB+1
                    ldaa      ARRTAB
                    jsr       ADD1
                    clra
                    ldab      #2
                    jsr       ADD1
                    jsr       PULLAE
                    cmpa      MEMEND
                    bls       DIM7
                    jmp       OVERFL

DIM7                staa      ARRTAB
                    stab      ARRTAB+1
                    jsr       SKIPSP
                    cmpa      #','
                    bne       DIMEX
                    inx
                    jmp       DIM1

DIMEX               cmpa      #$1E
                    bne       DIMERR
                    jmp       REMARK

STRSUB              jsr       PUSHX
                    ldx       DIMVAR
                    ldx       0,X
STRSU2              tst       0,X
                    beq       STRSU3
                    inx
                    bra       STRSU2

STRSU3              stab      0,X
                    jsr       PULLX
                    rts

FOR                 ldx       BASPNT
                    jsr       TSTV
                    bcc       FOR1
                    jmp       LET0

FOR1                stx       BASPNT
                    jsr       PULPSH
                    ldx       FORPNT
                    cpx       #FORSTK+48
                    bne       FOR11
                    ldab      #16
                    jmp       ERROR

FOR11               staa      0,X
                    inx
                    stab      0,X
                    inx
                    stx       FORPNT
                    ldx       BASPNT
                    jsr       SKIPSP
                    inx
                    cmpa      #'='
                    beq       FOR3
FOR2                jmp       LET2

FOR3                jsr       EXPR
                    jsr       STORE
                    inx
                    cmpa      #'T'
                    bne       FOR2
                    ldaa      0,X
                    inx
                    cmpa      #'O'
                    bne       FOR2
                    jsr       EXPR
                    jsr       PULLAE
                    stx       BASPNT
                    ldx       FORPNT
                    staa      0,X
                    inx
                    stab      0,X
                    inx
                    stx       FORPNT
                    ldx       BASPNT
                    ldaa      0,X
                    cmpa      #$1E
FOR8                bne       FOR2
                    inx
                    stx       BASLIN
                    ldx       FORPNT
                    ldaa      BASLIN
                    staa      0,X
                    inx
                    ldab      BASLIN+1
                    stab      0,X
                    inx
                    stx       FORPNT
                    jmp       BASIC

NEXT                ldx       BASPNT
                    jsr       TSTV
                    bcc       NEXT1
                    jmp       LET0

NEXT1               jsr       SKIPSP
                    cmpa      #$1E
                    bne       FOR8
                    inx
                    stx       BASLIN
                    ldx       #FORSTK
                    jsr       PULPSH
NEXT2               cpx       FORPNT
                    beq       NEXT6
                    cmpa      0,X
                    bne       NEXT5
                    cmpb      1,X
                    bne       NEXT5
                    jsr       IND
                    jsr       PULPSH
                    subb      3,X
                    sbca      2,X
                    bcs       NEXT4
                    stx       FORPNT
NEXT3               jmp       BASIC

NEXT4               jsr       PULLAE
                    addb      #1
                    adca      #0
                    jsr       PUSHX
                    ldx       0,X
                    staa      0,X
                    stab      1,X
                    jsr       PULLX
                    ldx       4,X
                    stx       BASLIN
                    bra       NEXT3

NEXT5               inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    bra       NEXT2

NEXT6               ldab      #17
                    jmp       ERROR

IF                  ldx       BASPNT
                    jsr       EXPR
                    bsr       RELOP
                    staa      NCMPR
                    jsr       EXPR
                    stx       BASPNT
                    bsr       CMPR
                    bcc       IF2
                    jmp       REMARK

IF2                 ldx       BASPNT
                    jsr       CCODE
                    ldx       0,X
                    jmp       0,X

RELOP               jsr       SKIPSP
                    inx
                    cmpa      #'='
                    bne       RELOP0
                    ldaa      #0
                    rts

RELOP0              ldab      0,X
                    cmpa      #'<'
                    bne       RELOP4
                    cmpb      #'='
                    bne       RELOP1
                    inx
                    ldaa      #2
                    rts

RELOP1              cmpb      #'>'
                    bne       RELOP3
RELOP2              inx
                    ldaa      #3
                    rts

RELOP3              ldaa      #1
                    rts

RELOP4              cmpa      #'>'
                    beq       REL44
                    ldab      #6
                    jmp       ERROR

REL44               cmpb      #'='
                    bne       RELOP5
                    inx
                    ldaa      #5
                    rts

RELOP5              cmpb      #'<'
                    beq       RELOP2
                    ldaa      #4
                    rts

CMPR                ldaa      NCMPR
                    asla
                    asla
                    staa      FUNNY+1
                    ldx       #CMPR1
                    jsr       SUB
                    jsr       PULLAE
                    tsta
FUNNY               jmp       0,X

CMPR1               beq       MAYEQ
                    bra       NOCMPR

                    bmi       OKCMPR
                    bra       NOCMPR

                    bmi       OKCMPR
                    bra       CMPR1

                    bne       OKCMPR
                    bra       MYNTEQ

                    beq       MYNTEQ
                    bmi       NOCMPR
                    bpl       OKCMPR
NOCMPR              sec
                    rts

OKCMPR              clc
                    rts

MAYEQ               tstb
                    beq       OKCMPR
                    bra       NOCMPR

MYNTEQ              tstb
                    bne       OKCMPR
                    bra       NOCMPR

;***************************************
;**** Replacement BREAK function for
;**** Corsham Tech 6800 w/ MP-S at $8004
;****
ACIACS              equ       $8004
ACIADA              equ       $8005

CBREAK1             psha
                    ldaa      ACIACS
                    asra      #                   ; WILL SET CARRY IF HAS DATA
                    bcc       CBREAK2
                    ldaa      ACIADA
                    cmpa      #$03
                    bne       CBREAK2
                    jmp       READY

CBREAK2             pula
                    rts

;****************************************

END                 equ       *
                    org       $A048
                    fdb       PROGM
                    end
