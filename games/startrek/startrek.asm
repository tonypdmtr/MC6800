; NAM STAR TREK
;*****************************
; STAR TREK GAME 1.2
; BY UNKNOWN SOURCE
; RECOVERED FROM OLD FLOPPIES
;
;*****************************

;
; MONITOR ROUTINE ADDRESSES
;
OUTCH               equ       $E1D1
INCH                equ       $E1AC
CONTRL              equ       $E0D0
;
; STORAGE AREAS
;
                    org       $20
STPSFL              rmb       1
BASEX               rmb       1
BASEY               rmb       1
BASESX              rmb       1
BASESY              rmb       1
GLMFLG              rmb       1
SECKLN              rmb       1
FLAGC               rmb       1
TIMDEC              rmb       1
STCFLG              rmb       1
SQFLG               rmb       1
PNTFLG              rmb       1
COURSE              rmb       1
WARP                rmb       1
FINCX               rmb       1
FINCY               rmb       1
COUNT               rmb       1
TEMP                rmb       2
TEMP2               rmb       2
XTEMP               rmb       2
TIME0               rmb       2
GAMTIM              rmb       1
TIMUSE              rmb       2
SHENGY              rmb       2
KLNENG              rmb       2
PHSENG              rmb       2
TOPFLG              rmb       1
BOTFLG              rmb       1
LSDFLG              rmb       1
RSDFLG              rmb       1
PHOTON              rmb       1
CURQUX              rmb       1
CURQUY              rmb       1
CURSCX              rmb       1
CURSCY              rmb       1
TRIALX              rmb       1
TRIALY              rmb       1
FLAG                rmb       1
CNDFLG              rmb       1
SCANX               rmb       1
SCANY               rmb       1
COUNT1              rmb       1
SECINF              rmb       1
MASK                rmb       1
KLNGCT              rmb       1
LENGTH              rmb       1
ASAVE               rmb       1
SHIELD              rmb       1
ENERGY              rmb       2
TSAVE1              rmb       1
HITKLS              rmb       1
HITSTR              rmb       1
HITBAS              rmb       1
TEMP3               rmb       2
GALCNT              rmb       1
DAMENG              rmb       1
DAMSRS              rmb       1
DAMLRS              rmb       1
DAMPHS              rmb       1
DAMPHT              rmb       1
DAMSHL              rmb       1
DAMTEL              rmb       1
DAMTRB              rmb       1
DAMCOM              rmb       1
PCOUNT              rmb       1                   ; TORP SPREAD COUNT
PTZFLG              rmb       1                   ; NO MORE TORP FLAG
GAMEND              rmb       1
AUTOSR              rmb       1
AUTOLR              rmb       1
SUPFLG              rmb       1
TELFLG              rmb       1
ATKENG              rmb       2
QUDPTR              rmb       2                   ; POINTS TO CURR LOC IN QUDMAP
PASWRD              rmb       3
PHTFLG              rmb       1
SHUTCR              rmb       1
SHUTLX              rmb       1
SHUTLY              rmb       1
QUDMAP              rmb       64
COMMAP              rmb       64                  ; MUST FOLLOW QUDMAP IMMEDIATELY
SECMAP              rmb       16                  ; PACKED SECTOR (64*2 BITS) 00=., 01=*, 10=K, 11=B
STUF                rmb       4                   ; SEED FOR RANDOM FUNCTION
;
; PROGRAM ENTRY
;
                    org       $0200
                    jmp       STRTRK

;
; TABLE OF MOVE VECTORS
;
MOVTBL              fcb       $00,$FF,$01,$FF,$01,$00,$01,$01  ; FF=-1
                    fcb       $00,$01,$FF,$01,$FF,$00,$FF,$FF
;
; CHARACTER PRINT TABLE
;
CHRTBL              fcc       '.*KBN'
;
; COMMAND CODE TABLE
;
CMDTBL              fcc       'ENSRLRPHPTDRSHTPSDTBCO'
;
; COMMAND JUMP TABLE
;
JMPTBL              fdb       SETCRS
                    fdb       SRSCAN
                    fdb       LRSCAN
                    fdb       PHASOR
                    fdb       PHOTOR
                    fdb       DAMRPT
                    fdb       SHLDS
                    fdb       TELEPT
                    fdb       SELFDE
                    fdb       TRCTBM
                    fdb       COMPTR
;
PDATA               jsr       OUTCH
                    inx
PDATA1              ldaa      0,X
                    cmpa      #$04
                    bne       PDATA
                    rts

;
PCRLF               ldaa      #$0D
                    jsr       OUTCH
                    ldaa      #$0A
                    jmp       OUTCH

;
PSTRNG              bsr       PCRLF
                    bra       PDATA1

;
OUTHL               psha
                    lsra
                    lsra
                    lsra
                    lsra
                    bsr       OUTHR
                    pula
                    rts

;
OUTHR               psha
                    anda      #$0F
                    oraa      #$30
                    cmpa      #$39
                    bls       OUTDIG
                    adda      #7
OUTDIG              jsr       OUTCH
                    pula
                    rts

;
OUTS                psha
                    ldaa      #$20
                    bra       OUTDIG

;
; LIB RANDOM
RANDOM              pshb                          ; RANDOM NUMBER GENERATOR - SAVE B
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

;
; PRINT TITLE
;
STRTRK              ldx       #TITLE
                    bsr       PSTRNG
;
; CLEAR ALL TEMP STORAGE
;
                    ldx       #STPSFL
SETCLR              clr       0,X
                    inx
                    cpx       #STUF               ; LAST VALUE OF RAM AREA
                    bne       SETCLR
                    ldaa      #$FF
                    ldx       #COMMAP
CLRMAP              staa      0,X
                    inx
                    cpx       #COMMAP+64
                    bne       CLRMAP
                    ldx       #SHTLNG
                    bsr       PSTRNG
                    jsr       INCH
                    cmpa      #'S'
                    beq       SETQD
                    inc       LENGTH              ; SET LONG FLAG
;
; SETUP SPACE
;
SETQD               ldx       #QUDMAP
                    ldab      #64
SETUP0              bsr       RANDOM
                    cmpa      #$FC
                    bls       SETUP1
                    ldaa      #4
                    bra       SETUP5

SETUP1              cmpa      #$F7
                    bls       SETUP2
                    ldaa      #3
                    bra       SETUP5

SETUP2              cmpa      #$E0
                    bls       SETUP3
                    ldaa      #2
                    bra       SETUP5

SETUP3              cmpa      #$A0
                    bls       SETUP4
                    ldaa      #1
                    bra       SETUP5

SETUP4              clra
SETUP5              clr       ASAVE
                    tst       LENGTH
                    beq       SETUP8
                    staa      ASAVE
                    jsr       RANDOM
                    cmpa      #$F0
                    bls       SETUP6
                    ldaa      #3
                    bra       SETUP8

SETUP6              cmpa      #$C0
                    bls       SETUP7
                    ldaa      #2
                    bra       SETUP8

SETUP7              clra
SETUP8              adda      ASAVE
                    staa      0,X                 ; STORE SECT KLNGON CNT
                    adda      KLNGCT
                    staa      KLNGCT
STARS               jsr       RANDOM
                    anda      #$38
                    oraa      0,X
                    staa      0,X
CONT                inx
                    decb
                    beq       CONT1
                    bra       SETUP0

;
; GET STARBASE LOCATION
;
CONT1               jsr       RANDOM
                    anda      #$7
                    tab
                    jsr       RANDOM
                    anda      #$7
                    staa      BASEX
                    stab      BASEY
                    inc       STPSFL
                    ldx       #QUDMAP
                    jsr       STPSEX
                    ldaa      #$40
                    oraa      0,X
                    staa      0,X
CONT2               bsr       REFUEL
                    jsr       RANDOM
                    adda      #0
                    daa
                    staa      TIME0+1
                    jsr       RANDOM
                    anda      #$7F
                    oraa      #$23
                    adda      #0
                    daa
                    staa      TIME0
                    bsr       MAKTIM
                    tst       LENGTH
                    beq       GATM
                    tab
                    bsr       MAKTIM
                    aba
                    daa
GATM                staa      GAMTIM
                    jsr       RANDOM              ; SET SHUTTLECRAFT LOCATION
                    anda      #$7
                    staa      SHUTLX
                    jsr       RANDOM
                    anda      #$7
                    staa      SHUTLY
                    bra       CONT3

;
; REFUEL THE ENTERPRISE
;
REFUEL              clr       SHIELD
                    ldaa      #$30
                    staa      ENERGY
                    clr       ENERGY+1
                    staa      SHENGY
                    clr       SHENGY+1
                    ldaa      #15
                    staa      PHOTON
                    ldx       #DAMENG             ; FIX ALL DAMAGE
REFUL1              clr       0,X
                    inx
                    cpx       #DAMENG+9
                    bne       REFUL1
                    rts

;
; CALCULATE GAME TIME
;
MAKTIM              jsr       RANDOM
                    anda      #$0F
                    oraa      #$31
                    adda      #0
                    daa
                    rts

;
; CONTINUE SETUP
;
CONT3               jsr       RANDOM
                    anda      #7
                    staa      CURQUX
                    jsr       RANDOM
                    anda      #7
                    staa      CURQUY
                    jsr       SETUPS
                    ldx       #BASINF
                    jsr       PSTRNG
                    ldaa      BASEX
                    bsr       FIXOUT
                    jsr       OUTDSH
                    ldaa      BASEY
                    bsr       FIXOUT
                    ldx       #INTRO0
                    jsr       PSTRNG
                    ldx       #PASWRD
                    ldab      #3
CONT4               jsr       INCH
                    staa      0,X
                    inx
                    decb
                    bne       CONT4
                    ldx       #INTRO1
                    jsr       PSTRNG
                    bsr       OUTDAT
                    ldx       #INTRO2
                    jsr       PSTRNG
                    bsr       OUTKLN
                    ldx       #INTRO3
                    jsr       PSTRNG
                    clr       TEMP
                    ldaa      GAMTIM
                    staa      TEMP+1
                    jsr       OUTBCD
                    ldx       #INTRO4
                    jsr       PSTRNG
                    bsr       OUTQUD
                    ldx       #INTRO6
                    jsr       PSTRNG
                    bsr       OUTSEC
                    bra       COMAND

;
; OUTPUT A NUMBER
;
FIXOUT              adda      #$31
                    jsr       OUTCH
                    rts

;
; OUTPUT STARDATE
;
OUTDAT              ldx       TIME0
                    stx       TEMP
                    jsr       OUTBCD
                    ldaa      #'.'
                    jsr       OUTCH
                    ldaa      TIMDEC
                    jsr       OUTHR
                    rts

;
; OUTPUT A KLINGON COUNT
;
OUTKLN              ldaa      KLNGCT
OUTK0               clr       TEMP
OUTK1               clr       TEMP+1
                    ldab      #10
OUTK2               sba
                    bcs       OUTK3
                    inc       TEMP+1
                    cmpb      TEMP+1
                    bne       OUTK2
                    inc       TEMP
                    bra       OUTK1

OUTK3               aba
                    ldab      TEMP+1
                    aslb
                    aslb
                    aslb
                    aslb
                    aba
                    staa      TEMP+1
                    jsr       OUTBCD
                    rts

;
; OUTPUT QUADRANT LOCATION
;
OUTQUD              ldaa      CURQUY
                    bsr       FIXOUT
                    bsr       OUTDSH
                    ldaa      CURQUX
                    bsr       FIXOUT
                    rts

;
; OUTPUT A SECTOR
;
OUTSEC              ldaa      CURSCY
                    bsr       FIXOUT
                    bsr       OUTDSH
                    ldaa      CURSCX
                    bsr       FIXOUT
                    rts

;
; OUTPUT A DASH
;
OUTDSH              ldaa      #'-'
                    jsr       OUTCH
                    rts

;
; ADD THE A-REG TO THE INDEX REGISTER
;
FIXXRG              stx       TEMP
                    adda      TEMP+1
                    staa      TEMP+1
                    ldx       TEMP
                    rts

;
; GET COMMAND AND PERFORM IT
;
COMAND              ldaa      GAMTIM
                    cmpa      TIMUSE+1
                    bhi       NOEXTC
                    jmp       NOMTIM

NOEXTC              tst       KLNGCT
                    bne       NOEXT2
                    jmp       NOMKLN

NOEXT2              tst       SUPFLG
                    beq       NOEXT4
                    ldx       #SUPDES
                    jsr       PSTRNG
                    jsr       SELFDA
                    jmp       ENDGAM

;
NOEXT4              jsr       CLRCQU              ; CLEAR K & S
                    ldaa      #2
                    cmpa      CNDFLG              ; DOCKED?
                    beq       CMND27
                    clr       CNDFLG
                    tst       SECKLN              ; RED?
                    beq       CMNDAC
                    deca
                    staa      CNDFLG
CMNDAC              jsr       RANDOM
                    cmpa      #$FC                ; SPACE STORM
                    bls       COMND2
                    ldx       #SPSTRM
                    jsr       PSTRNG
                    ldaa      #2
                    staa      DAMSHL
                    jsr       SHLDWN
;
COMND2              jsr       RANDOM
                    cmpa      #$FC                ; SUPERNOVA?
                    bls       CMND25
                    jsr       SUPNOV
CMND25              jsr       ATTACK              ; ALLOW ATTACK
                    tst       ENERGY
                    bpl       COMND0
                    jmp       NRGOUT

;
COMND0              tst       SHUTCR
                    bne       CMND01
                    ldaa      SHUTLX
                    cmpa      CURQUX
                    bne       CMND01
                    ldaa      SHUTLY
                    cmpa      CURQUY
                    bne       CMND01
                    ldx       #SHTSIG
                    jsr       PSTRNG
CMND01              ldaa      #3
                    cmpa      ENERGY
                    bls       CMNDAD
                    staa      CNDFLG
CMNDAD              tst       SHENGY
                    bpl       CMND27
                    clra
                    staa      SHIELD
                    staa      SHENGY
                    staa      SHENGY+1
CMND27              ldx       #COMST              ; PRINT COMMAND PROMPT
                    jsr       PSTRNG
                    clr       STCFLG
                    clr       PHTFLG
COMND3              jsr       INCH
                    cmpa      #$D
                    beq       ILCMND
                    tab
                    jsr       INCH
                    ldx       #CMDTBL
CHKCM1              cmpb      0,X
                    bne       INX2
                    cmpa      1,X
                    beq       GOTCMD
INX2                inx
                    inx
                    cpx       #CMDTBL+22
                    bne       CHKCM1
ILCMND              ldx       #EXPCMD
                    jsr       PSTRNG
                    bra       COMND3

;
GOTCMD              ldaa      #22                 ; STEP FORWARD 22 BYTES TO ROUTINE ADDRESS
GCMD1               inx
                    deca
                    bne       GCMD1
                    ldx       0,X
                    jsr       0,X
                    tst       GAMEND
                    beq       CMND99
                    jmp       ENDGAM

CMND99              jmp       COMAND

;
; OUTPUT A 4 DIGIT BCD NUMBER
;
OUTBCD              clr       FLAG
                    ldaa      TEMP
                    beq       OUTBC2
                    anda      #$F0
                    beq       OUTBC1
                    ldaa      TEMP
                    jsr       OUTHL
OUTBC1              ldaa      TEMP
                    anda      #$0F
                    jsr       OUTHR
                    inc       FLAG
OUTBC2              ldaa      TEMP+1
                    tst       FLAG
                    bne       NOZERO
                    anda      #$F0
                    beq       OUTBC3
NOZERO              jsr       OUTHL
OUTBC3              ldaa      TEMP+1
                    anda      #$0F
                    jsr       OUTHR
                    rts

;
; LOWER THE SHIELDS
;
SHLDS               tst       SHIELD
                    beq       SHLDUP
SHLDWN              clr       SHIELD
                    ldx       #DWNST
SHLD0               jsr       PSTRNG
SHLD1               rts

;
; RAISE THE SHIELDS
;
SHLDUP              ldaa      SHENGY
                    cmpa      #1
                    bls       SHLD1
                    tst       DAMSHL
                    bne       SHLDWN
                    inc       SHIELD
                    ldx       #SHENGY
                    ldaa      #2
                    staa      TEMP
                    clr       TEMP+1
                    jsr       BCDSUB
                    ldx       #UPSTR
                    bra       SHLD0

;
; SHORT RANGE SCAN
;
SRSCAN              tst       DAMSRS
                    beq       SSCAN1
                    tst       SHUTCR
                    bne       SSCAN0
                    jsr       RPTDAM
                    rts

SSCAN0              ldx       #SCBKUP
                    jsr       PSTRNG
SSCAN1              jsr       PCRLF
                    clr       SCANY
                    ldaa      CNDFLG
                    cmpa      #2
                    bne       SSCAN2
                    jsr       REFUEL
SSCAN2              ldx       #SECMAP
                    stx       TEMP2
                    jsr       DOSCAN
                    ldx       #SDATE
                    jsr       PDATA1
                    jsr       OUTDAT
                    jsr       DOSCAN
                    ldx       #CNDTNS
                    jsr       PDATA1
                    ldaa      CNDFLG
                    beq       SRSCN1
                    cmpa      #1
                    beq       SRSCN0
                    cmpa      #3
                    beq       OUTCN1
                    ldx       #DOCKED
                    bra       OUTCND

OUTCN1              ldx       #YELLOW
OUTCND              jsr       PDATA1
                    bra       SRSCN2

;
SRSCN0              ldx       #RED
                    bra       OUTCND

;
SRSCN1              ldx       #GREEN
                    bra       OUTCND

;
SRSCN2              bsr       DOSCAN
                    ldx       #QUADP
                    jsr       PDATA1
                    jsr       OUTQUD
                    bsr       DOSCAN
                    ldx       #SECP
                    jsr       PDATA1
                    jsr       OUTSEC
                    bsr       DOSCAN
                    ldx       #ENGSTR
                    jsr       PDATA1
                    ldx       ENERGY
                    stx       TEMP
                    jsr       OUTBCD
                    bsr       DOSCAN
                    ldx       #KLSTR
                    jsr       PDATA1
                    jsr       OUTKLN
                    bsr       DOSCAN
                    ldx       #SHSTR
                    jsr       PDATA1
                    ldx       SHENGY
                    stx       TEMP
                    jsr       OUTBCD
                    tst       SHIELD
                    beq       SRSCN4
                    ldx       #UPSCAS
                    bra       SRSCN5

;
SRSCN4              ldx       #DNSCAS
SRSCN5              jsr       PDATA1
                    bsr       DOSCAN
                    ldx       #TRPSTR
                    jsr       PDATA1
                    clr       TEMP
                    ldaa      PHOTON
                    adda      #0
                    daa
                    staa      TEMP+1
                    jsr       OUTBCD
                    ldx       QUDPTR              ; UPDATE COMPUTER MAP
                    ldaa      0,X
                    jsr       UPDCMP
                    rts

;
; OUTPUT 1 SHORT RANGE SCAN LINE
;
DOSCAN              jsr       PCRLF
                    ldaa      #2
                    staa      COUNT
                    clr       SCANX
DOSCN               ldaa      #4
                    staa      COUNT1
                    ldx       TEMP2
                    ldaa      0,X
DOSCN0              staa      ASAVE
                    ldaa      CURSCY
                    cmpa      SCANY               ; IS IT Y-LOC OF ENTERPRISE?
                    bne       CHK0
                    ldaa      CURSCX
                    cmpa      SCANX
                    bne       CHK0
                    ldaa      #'E'
                    jsr       OUTCH
                    bra       GOAHD

CHK0                ldaa      ASAVE
DOSCN1              ldx       #CHRTBL
                    anda      #3
                    jsr       FIXXRG
                    ldaa      0,X
                    jsr       OUTCH
GOAHD               jsr       OUTS
                    ldx       TEMP2
                    inc       SCANX
                    dec       COUNT1
                    beq       DOSCN2
                    ldaa      ASAVE
                    lsra
                    lsra
                    bra       DOSCN0

DOSCN2              inx
                    stx       TEMP2
                    ldaa      0,X
                    dec       COUNT
                    bne       DOSCN
                    inc       SCANY
                    rts

;
; SETUP SECTOR MAP
;
SETUPS              clr       SECKLN
                    ldx       #SECMAP
                    ldab      #16
                    clra
SETPS1              staa      0,X
                    inx
                    decb
                    bne       SETPS1
                    ldx       #QUDMAP
                    ldaa      CURQUX
                    ldab      CURQUY
STPSEX              staa      ASAVE
                    tstb
                    beq       SETPS4
SETPS2              ldaa      #8
SETPS3              inx
                    deca
                    bne       SETPS3
                    decb
                    bne       SETPS2
SETPS4              ldaa      ASAVE
                    beq       SETPS6
SETPS5              inx
                    deca
                    bne       SETPS5
SETPS6              tst       STPSFL
                    beq       STPSNX
                    clr       STPSFL
                    rts

;
STPSNX              stx       QUDPTR
                    ldab      0,X
                    stab      SECINF
                    beq       SETP10
                    andb      #7
                    beq       SETPS7
                    stab      COUNT
                    stab      SECKLN
                    ldaa      #2
                    staa      MASK
                    bsr       PUTINM
SETPS7              ldab      SECINF
                    andb      #$38
                    beq       SETPS8
                    lsrb
                    lsrb
                    lsrb
                    stab      COUNT
                    ldaa      #1
                    staa      MASK
                    bsr       PUTINM
SETPS8              ldab      SECINF
                    bitb      #$40
                    beq       SETPS9
                    ldaa      #1
                    staa      COUNT
                    ldaa      #3
                    staa      MASK
                    bsr       PUTINM
SETPS9              ldab      SECINF
                    bpl       SETP10
                    inc       SUPFLG
SETP10              jsr       RANDOM
                    anda      #7
                    staa      TRIALX
                    jsr       RANDOM
                    anda      #7
                    staa      TRIALY
                    bsr       CHKPOS
                    tst       FLAG
                    bne       SETP10
                    ldaa      TRIALX
                    staa      CURSCX
                    ldaa      TRIALY
                    staa      CURSCY
;
; COMPUTE LOCAL KLINGON ENERGY
;
CSCKEN              ldab      SECKLN
                    aslb
                    ldx       #KLNENG
CSCEXT              clr       0,X
                    clr       1,X
CSCKN1              jsr       RANDOM
                    adda      #0
                    daa
                    clr       TEMP
                    staa      TEMP+1
                    jsr       BCDADD
                    decb
                    bne       CSCKN1
                    rts

;
; PUT OBJECTS IN SECTOR MAP
;
PUTINM              ldx       #SECMAP
                    jsr       RANDOM
                    anda      #$F
                    staa      TSAVE1
                    jsr       FIXXRG
                    ldab      0,X
                    jsr       RANDOM
                    anda      #3
                    staa      ASAVE
                    beq       PUTIN2
PUTIN1              rorb
                    rorb
                    deca
                    bne       PUTIN1
PUTIN2              bitb      #3
                    bne       PUTINM
                    orab      MASK
                    ldaa      ASAVE
                    beq       PUTIN4
PUTIN3              rolb
                    rolb
                    deca
                    bne       PUTIN3
PUTIN4              stab      0,X
                    dec       COUNT
                    bne       PUTINM
                    ldaa      MASK
                    cmpa      #3
                    bne       PUTIN6
                    ldab      TSAVE1
                    clc
                    rorb
                    ldaa      ASAVE
                    bcc       PUTIN5
                    adda      #4
PUTIN5              staa      BASESX
                    stab      BASESY
PUTIN6              rts

;
; CHECK FOR EMPTY POSITIONS
;
CHKPOS              clr       FLAG
                    stx       XTEMP
                    ldx       #SECMAP
                    ldaa      TRIALY
                    beq       CHKPO2
CHKPO1              inx
                    inx
                    deca
                    bne       CHKPO1
CHKPO2              ldaa      TRIALX
                    cmpa      #3
                    bls       CHKPO3
                    inx
                    suba      #4
CHKPO3              ldab      0,X
                    staa      ASAVE
                    beq       CHKPO5
CHKPO4              lsrb
                    lsrb
                    deca
                    bne       CHKPO4
CHKPO5              andb      #3
                    beq       CHKPO6
                    stab      FLAG
                    tst       STCFLG
                    beq       CHKPO6
                    tst       PHTFLG
                    bne       CHKPO7
                    cmpb      #2
                    beq       CHKPO7
CHKPO6              ldx       XTEMP
                    rts

;
CHKPO7              ldab      #$FC
                    ldaa      ASAVE
                    beq       CHKPO9
                    sec
CHKPO8              rolb
                    rolb
                    deca
                    bne       CHKPO8
CHKPO9              andb      0,X
                    stab      0,X
                    bra       CHKPO6

;
; FIRE PHOTON TORPEDO
;
PHOTOR              tst       DAMPHT
                    beq       PTRNDM
PTRND9              jsr       RPTDAM
                    rts

PTRNDM              tst       PHOTON              ; ANY LEFT
                    bne       PHOTRO
PTEMPT              ldx       #PTEMST
                    jsr       PSTRNG
                    inc       PTZFLG
                    rts

;
PHOTRO              inc       PHTFLG
                    jsr       RANDOM
                    anda      #$F
                    oraa      #4
                    staa      WARP
                    dec       PHOTON
;
; WARP ENGINES AND PHOTON TORP COURSE
;
SETCRS              clr       SQFLG
                    clr       GLMFLG
                    ldx       #CRSSTR
                    jsr       PSTRNG
                    jsr       INCHCK
                    cmpa      #7
                    bhi       ABC29
                    jsr       OUTS
                    jsr       OUTS
                    staa      COURSE
                    tst       PHTFLG
                    bne       PHTOR1
                    ldx       #WRPSTR
                    jsr       PSTRNG
                    jsr       INCHCK
                    staa      WARP
                    tst       PNTFLG
                    bne       STCRS2
                    staa      SQFLG
                    tst       DAMENG
                    bne       PTRND9
;
STCRS2              jsr       INCH
                    cmpa      #$0D
                    bne       STCRS2
                    ldaa      COURSE
PHTOR1              ldx       #MOVTBL
                    asla
                    jsr       FIXXRG
                    ldaa      WARP
                    bne       ABC30
ABC29               rts

;
ABC30               staa      COUNT
                    tst       SQFLG
                    beq       STCRS3
                    ldaa      #$0F
                    staa      COUNT
STCRS3              ldaa      CURSCX
                    ldab      CURSCY
PHTOR2              adda      0,X
                    addb      1,X
                    jsr       TSTBND
                    tst       FINCX
                    beq       ABC1
                    jmp       STCRS5

;
ABC1                tst       FINCY
                    beq       ABC5
                    jmp       STCRS5

;
ABC5                staa      TRIALX              ; SAVE TRIAL POSITION
                    stab      TRIALY
                    tst       PHTFLG
                    beq       ABC4
                    jsr       OUTS
                    bsr       OBATSO
ABC4                inc       STCFLG
                    jsr       CHKPOS              ; CHK IF BLKD
                    ldaa      FLAG
                    beq       STCRS4
                    tst       PHTFLG
                    beq       ABC2
                    jmp       PHTOR3

;
ABC2                cmpa      #2
                    bne       ABC3
                    jmp       KLGRAM

;
ABC3                ldx       #BLOKST
                    jsr       PSTRNG
OBATSO              ldaa      TRIALY
                    jsr       FIXOUT
                    jsr       OUTDSH
                    ldaa      TRIALX
                    jsr       FIXOUT
                    tst       PHTFLG
                    beq       STCRET
                    rts

;
STCRET              jmp       STCRS6

;
STCRS4              tst       PHTFLG
                    beq       STCRSB              ; JUMP IF NOT
                    ldaa      TRIALX
                    ldab      TRIALY
                    dec       COUNT
                    bne       PHTOR2
PHTOR4              ldx       #PNOENG
                    jsr       PSTRNG
                    bra       STCRS6

;
STCRSB              ldaa      TRIALX
                    staa      CURSCX
                    ldaa      TRIALY
                    staa      CURSCY
                    jsr       RANDOM
                    cmpa      #$80
                    bls       STCRSC
                    ldaa      #1
                    jsr       FIXTIM
                    ldaa      #3
                    jsr       FIXENG
STCRSC              dec       COUNT               ; DEC MOVE CNTR
                    beq       STCRSD
                    jmp       STCRS3

;
STCRSD              tst       SQFLG
                    bne       STCRS5              ; QUADRANT MOVE!
                    bra       STCRS6

;
STCRS5              tst       PHTFLG
                    beq       ABC6
                    bra       PHTOR4

ABC6                ldaa      WARP
                    staa      COUNT
ABC7                ldaa      CURQUX
                    ldab      CURQUY
                    tst       SQFLG
                    beq       STCRS7
                    adda      0,X
                    addb      1,X
                    staa      TRIALX
                    stab      TRIALY
                    jsr       TSTBND
                    tst       FINCX
                    beq       ABCD0
                    jmp       GALBND

;
ABCD0               tst       FINCY
                    beq       ABCD1
                    jmp       GALBND

;
ABCD1               ldaa      TRIALX
                    staa      CURQUX
                    ldaa      TRIALY
                    staa      CURQUY
                    inc       GLMFLG
                    ldaa      #6
                    jsr       FIXTIM
                    ldaa      #$30
                    jsr       FIXENG
                    dec       COUNT
                    bne       ABC7
STCRSA              jsr       SETUPS
STCRS6              clr       CNDFLG
                    ldaa      BASEX
                    cmpa      CURQUX
                    bne       SEXIT1
                    ldab      BASEY
                    cmpb      CURQUY
                    bne       SEXIT1
                    ldaa      BASESX
                    ldab      BASESY              ; DOCKED?
                    inca
                    suba      CURSCX
                    cmpa      #2
                    bhi       SEXIT1
SEXIT0              incb
                    subb      CURSCY
                    cmpb      #2
                    bhi       SEXIT1
SDOCK               ldaa      #2
                    staa      CNDFLG
SEXIT1              tst       AUTOSR
                    beq       SEXIT2
                    jsr       SRSCAN
SEXIT2              tst       AUTOLR
                    beq       SEXIT3
                    jsr       LRSCAN
SEXIT3              rts

;
STCRS7              adda      FINCX
                    addb      FINCY
                    cmpa      #7
                    bhi       GALBND
                    cmpb      #7
                    bhi       GALBND
                    staa      CURQUX
                    stab      CURQUY
                    ldaa      #7
                    jsr       FIXTIM
                    bra       STCRSA

;
; RAMMED A KLINGON ROUTINE
;
KLGRAM              ldx       #KRMSTR
                    jsr       PSTRNG
                    ldaa      #1
                    staa      COUNT
                    clr       SQFLG
                    inc       HITKLS
                    dec       KLNGCT              ; DEC KLINGON COUNT
                    dec       SECKLN
                    inc       PHTFLG
                    jsr       OBATSO
                    ldx       #HEVDAM
                    jsr       PSTRNG
                    ldab      #$6A
                    jsr       MANDAM
                    ldx       #STILFT
                    jsr       PSTRNG
                    jsr       OUTKLN
                    jmp       STCRSB

;
; PRINT GALAXY LIMIT MESSAGE
;
GALBND              ldx       #GLBNDS
                    jsr       PSTRNG
                    ldaa      GALCNT
                    inca
                    cmpa      #3
                    bne       GALBN2
                    ldx       #GALDUM
                    jsr       PSTRNG
                    inc       GAMEND
                    rts

;
GALBN2              staa      GALCNT
                    tst       GLMFLG
                    bne       ABC20
                    jmp       STCRS6

;
ABC20               jmp       STCRSA

;
; TORPEDO HAS HIT SOMETHING
;
PHTOR3              ldx       #PHITST
                    jsr       PSTRNG
                    ldaa      FLAG
                    cmpa      #2                  ; KLINGON?
                    bne       ABC8
                    dec       SECKLN
                    dec       KLNGCT
                    inc       HITKLS
                    ldx       #KLGSTR
                    jsr       PDATA1
                    ldx       #STILFT
                    jsr       PSTRNG
                    jsr       OUTKLN
                    bra       ABC10

;
ABC8                cmpa      #1                  ; STAR?
                    bne       ABC11
                    inc       HITSTR
                    ldx       #STARST
ABC9                jsr       PDATA1
ABC10               rts

;
ABC11               ldx       #BASEST             ; HIT BASE!
                    inc       HITBAS
                    bra       ABC9

;
; SEE IF GALAXY EDGE REACHED
;
TSTBND              clr       FINCX
                    clr       FINCY
                    tsta
                    bpl       TSTBN1
                    dec       FINCX
                    bra       TSTBN2

;
TSTBN1              cmpa      #7
                    bls       TSTBN2
                    inc       FINCX
TSTBN2              tstb
                    bpl       TSTBN3
                    dec       FINCY
                    rts

;
TSTBN3              cmpb      #7
                    bls       TSTBN4
                    inc       FINCY
TSTBN4              rts

;
; INPUT CHARACTER AND CHECK
;
INCHCK              clra
INCHK0              staa      PNTFLG
                    jsr       INCH
                    cmpa      #'.'
                    beq       INCHK0
                    cmpa      #'9'
                    bhi       INCHK1
                    cmpa      #'0'-1
                    bls       INCHK1
                    suba      #$30
                    rts

;
INCHK1              ldx       #ERR
                    jsr       PDATA1
                    bra       INCHCK

;
; ADD TO GAME TIME
;
FIXTIM              adda      TIMDEC
                    cmpa      #9
                    bls       FIXTM1
                    suba      #10
                    staa      TIMDEC
                    clr       TEMP
                    ldaa      #1
                    staa      TEMP+1
                    stx       XTEMP
                    ldx       #TIME0
                    bsr       BCDADD
                    ldx       #TIMUSE
                    bsr       BCDADD
                    jsr       FIXDAM
                    ldx       XTEMP
                    rts

;
FIXTM1              staa      TIMDEC
                    rts

;
; *SUBTRACT FROM ENERGY AMOUNT
;
FIXENG              stx       XTEMP
                    ldx       #ENERGY
                    clr       TEMP
                    staa      TEMP+1
                    bsr       BCDSUB
                    ldx       XTEMP
                    rts

;
; BCD ADDITION
;
BCDADD              clc
                    bra       BCDFIX

;
; BCD SUBTRACTION
;
BCDSUB              ldaa      #$99
                    suba      TEMP
                    staa      TEMP
                    ldaa      #$99
                    suba      TEMP+1
                    staa      TEMP+1
                    sec
;
BCDFIX              ldaa      1,X
                    adca      TEMP+1
                    daa
                    staa      1,X
                    ldaa      0,X
                    adca      TEMP
                    daa
                    staa      0,X
                    rts

;
; LONG RANGE SCAN
;
LRSCAN              tst       DAMLRS
                    beq       LRSNDM
                    jsr       RPTDAM
                    rts

;
LRSNDM              clr       TOPFLG
                    clr       BOTFLG
                    clr       LSDFLG
                    clr       RSDFLG
                    ldx       #LRSCST
                    jsr       PSTRNG
                    jsr       OUTQUD
                    jsr       PCRLF
                    ldaa      CURQUX
                    bne       LRSCNT
                    inc       LSDFLG
LRSCNT              cmpa      #7
                    bne       LRSCN2
                    inc       RSDFLG
LRSCN2              ldaa      CURQUY
                    bne       LRSCN3
                    inc       TOPFLG
LRSCN3              cmpa      #7
                    bne       LRSCN4
                    inc       BOTFLG
LRSCN4              ldx       QUDPTR
LRSCNC              ldaa      #$F7
                    jsr       FIXXRG
                    tst       TOPFLG
                    beq       LRSCN7
                    bsr       OUTTH0
                    bra       LRSCN8

;
LRSCN7              bsr       OUTLIN
LRSCN8              ldaa      #5
                    jsr       FIXXRG
                    bsr       OUTLIN
                    ldaa      #5
                    jsr       FIXXRG
                    tst       BOTFLG
                    beq       LRSCN9
                    bsr       OUTTH0
                    bra       LRSC10

;
LRSCN9              bsr       OUTLIN
LRSC10              rts

;
; OUTPUT 1 LINE OF LONG RANGE SCAN
;
OUTLIN              ldaa      0,X
                    tst       LSDFLG
                    beq       OUTLN1
                    clra
                    bra       OUTLN2

OUTLN1              bsr       UPDCMP
OUTLN2              bsr       OUTQIN
                    ldaa      0,X
                    bsr       UPDCMP
                    bsr       OUTQIN
                    ldaa      0,X
                    tst       RSDFLG
                    beq       OUTLN3
                    clra
                    bra       OUTLN4

OUTLN3              bsr       UPDCMP
OUTLN4              bsr       OUTQIN
                    jsr       PCRLF
                    rts

;
; UPDATE COMPUTER HISTORY MAP
;
UPDCMP              staa      64,X
                    rts

;
; OUTPUT QUADRANT INFORMATION
;
OUTQIN              tab
                    anda      #$80
                    clc
                    rola
                    rola
                    jsr       OUTHR
                    tba
                    anda      #$40
                    lsra
                    lsra
                    jsr       OUTHL
                    tba
                    anda      #$38                ; STARS
                    asla
                    jsr       OUTHL
                    tba
                    anda      #$07                ; KLINGONS
                    jsr       OUTHR
                    inx
                    jsr       OUTS
                    rts

;
; OUTPUT A LINE OF ZEROS
;
OUTTH0              ldaa      #3
                    staa      COUNT
OUTTH1              clra
                    bsr       OUTQIN
                    dec       COUNT
                    bne       OUTTH1
                    jsr       PCRLF
                    rts

;
; FIRE PHASORS
;
PHASOR              tst       DAMPHS
                    beq       PHSNDM
                    jsr       RPTDAM
                    rts

;
PHSNDM              tst       SHIELD
                    beq       PHASR1
                    ldx       #MLSHLD
                    bra       TOOMC1

;
PHASR1              ldx       #ENAVLB             ; REPORT ENERGY
                    jsr       PSTRNG
                    ldx       ENERGY
                    stx       TEMP
                    jsr       OUTBCD
                    ldx       #FIRENG
                    jsr       PSTRNG
                    jsr       INBCD
                    ldaa      PHSENG
                    cmpa      ENERGY
                    bhi       TOOMCH
                    bne       PHASR2
                    ldaa      PHSENG+1
                    cmpa      ENERGY+1
                    bls       PHASR2
TOOMCH              ldx       #TOMUCH
TOOMC1              jsr       PSTRNG
                    rts

;
PHASR2              jsr       RANDOM
                    cmpa      #$F4
                    bls       PHASR3
                    ldx       #PHAMIS
                    jsr       PSTRNG
                    bra       PHASR6

;
PHASR3              ldaa      PHSENG
                    cmpa      KLNENG
                    bhi       PHASR4
                    bne       PHASR5
                    ldaa      PHSENG+1
                    cmpa      KLNENG+1
                    bls       PHASR5
PHASR4              clr       KLNENG
                    clr       KLNENG+1
                    ldx       #ALKILL
                    jsr       PSTRNG
                    ldx       #STILFT
                    jsr       PSTRNG
                    ldab      SECKLN
                    stab      HITKLS
                    ldaa      KLNGCT
                    sba
                    staa      KLNGCT
                    jsr       OUTKLN
                    clr       SECKLN
KILALK              ldx       #SECMAP
                    ldaa      #16
                    staa      COUNT
KILAL1              ldab      #4
                    ldaa      0,X
KILAL2              rora
                    bcs       KILAL4
                    rora
                    bcc       KILAL3
                    clc
KILAL3              decb
                    bne       KILAL2
                    rora
                    staa      0,X
                    inx
                    dec       COUNT
                    bne       KILAL1
                    bra       PHASR6

;
KILAL4              rora
                    bra       KILAL3

;
PHASR5              ldx       #PHSENG
                    stx       TEMP
                    ldx       #KLNENG
                    jsr       BCDSUB
                    ldx       #KHTADM
                    jsr       PSTRNG
PHASR6              ldx       PHSENG
                    stx       TEMP
                    ldx       #ENERGY
                    jsr       BCDSUB
                    rts

;
; INPUT A BCD NUMBER
;
INBCD               clr       PHSENG
                    clr       PHSENG+1
INBCD1              jsr       INCH
                    cmpa      #'0'
                    blo       EXIT
                    bsr       CHECK
                    tst       FLAGC
                    bne       INEROR
                    ldab      #4
INBCD2              asl       PHSENG+1
                    rol       PHSENG
                    decb
                    bne       INBCD2
                    adda      PHSENG+1
                    staa      PHSENG+1
                    bra       INBCD1

;
INEROR              ldx       #ERR
                    jsr       PDATA1
                    bra       INBCD1

;
EXIT                rts

;
CHECK               clr       FLAGC
                    cmpa      #'9'
                    bhi       SETFLG
                    anda      #$0F
                    rts

;
SETFLG              inc       FLAGC
                    rts

;
; SELF DESTRUCT ROUTINE
;
SELFDE              ldx       #ABORT1
                    jsr       PSTRNG
                    ldx       #PASWRD
                    ldab      #3
SELFD1              jsr       INCH
                    cmpa      0,X
                    bne       SELFD2
                    inx
                    decb
                    bne       SELFD1
SELFDA              ldx       #DISINT
                    jsr       PSTRNG
                    inc       GAMEND
                    rts

;
SELFD2              ldx       #ABORT2
                    jsr       PSTRNG
                    rts

;
; TELEPORT ROUTINE
;
TELEPT              ldaa      #$12
                    cmpa      TIMUSE+1
                    bhi       TELEP2
                    tst       TELFLG
                    bne       TELEP4
                    jsr       RANDOM
                    cmpa      #$B0                ; POSS DAMAGED
                    bls       TELEP1
                    inc       TELFLG
TELEP1              jsr       RANDOM              ; MALFUNCTION?
                    cmpa      #$80
                    bhi       TELEP5
                    ldaa      BASEX
                    staa      CURQUX
                    ldaa      BASEY
                    staa      CURQUY
TELEPA              jmp       STCRSA

;
TELEP2              ldx       #CANTUS
TELEP3              jsr       PSTRNG
                    rts

;
TELEP4              ldx       #DMGDST
                    bra       TELEP3

;
TELEP5              jsr       RANDOM
                    anda      #7
                    staa      CURQUX
                    jsr       RANDOM
                    anda      #7
                    staa      CURQUY
                    ldx       #SOMWHR
                    jsr       PSTRNG
                    bra       TELEPA

;
; KLINGON ATTACK ROUTINE
;
ATTACK              tst       SECKLN              ; ANY Ks?
                    bne       ATTAC1
                    rts

;
ATTAC1              jsr       RANDOM              ; MAY NOT ATTACK
                    cmpa      #$B0
                    bhi       ATTAC2
                    ldx       #ATKENG
                    ldab      SECKLN
                    aslb
                    jsr       CSCEXT
                    ldx       ATKENG
                    stx       TEMP
                    tst       SHIELD
                    bne       ATTAC3
                    ldx       #ENERGY
                    jsr       BCDSUB
                    jsr       PCRLF
                    ldx       ATKENG
                    stx       TEMP
                    jsr       OUTBCD
                    ldx       #KATKDN
                    jsr       PDATA1
                    ldab      #$FA
                    jsr       MANDAM
ATTAC2              rts

;
ATTAC3              ldx       #SHENGY
                    jsr       BCDSUB
                    ldx       #KATKUP
                    jsr       PSTRNG
                    rts

;
; END OF GAME CLEANUP ROUTINE
;
NRGOUT              ldx       #NMENGS
NRGOU1              jsr       PSTRNG
                    bra       ENDGAM

;
NOMTIM              ldx       #NMTMST
                    bra       NRGOU1

;
NOMKLN              ldx       #NMKLST
                    jsr       PSTRNG
                    bra       ENDGM2

;
ENDGAM              ldx       #FAILST
                    jsr       PSTRNG
                    bra       ENDGM3

;
ENDGM2              ldx       #SUCCST
                    jsr       PSTRNG
ENDGM3              ldx       #PLAYAG
                    jsr       PSTRNG
                    jsr       INCH
                    cmpa      #'Y'
                    beq       ENDGM4
                    jmp       CONTRL

;
ENDGM4              jmp       STRTRK

;
; CLEAR OUT CURRENT QUADRANT
;
CLRCQU              ldx       QUDPTR
                    ldaa      0,X
                    suba      HITKLS              ; CLEAR Ks
                    ldab      HITSTR              ; CLEAR Ss
                    aslb
                    aslb
                    aslb
                    sba
                    tst       HITBAS              ; CLEAR B?
                    beq       CLRCQ2
                    anda      #$BF
                    ldab      #$A
                    stab      BASEX
                    stab      BASEY
                    clr       CNDFLG
CLRCQ2              staa      0,X
                    clra
                    staa      HITKLS
                    staa      HITSTR
                    staa      HITBAS
                    rts

;
; FIX DAMAGE ROUTINE
;
FIXDAM              ldx       #DAMENG
FIXDM1              tst       0,X
                    beq       FIXDM2
                    dec       0,X
FIXDM2              inx
                    cpx       #DAMENG+9
                    bne       FIXDM1
                    rts

;
; SUPERNOVA GENERATOR
;
SUPNOV              jsr       RANDOM
                    anda      #7
                    tab
                    jsr       RANDOM
                    anda      #7
                    stab      TSAVE1
                    ldx       #QUDMAP
                    inc       STPSFL
                    jsr       STPSEX
                    ldab      0,X
                    andb      #7                  ; CLEAR Ks
                    ldaa      KLNGCT
                    sba
                    staa      KLNGCT
                    ldaa      #$80
                    staa      0,X
                    ldx       #SUPSTR
                    jsr       PSTRNG
                    ldaa      TSAVE1
                    jsr       FIXOUT
                    jsr       OUTDSH
                    ldaa      ASAVE
                    jsr       FIXOUT
                    rts

;
; GENERATE MAIN DAMAGE
;
MANDAM              ldx       #DAMENG
MANDM1              jsr       RANDOM
                    cba
                    bls       MANDM2
                    jsr       RANDOM
                    anda      #3
                    sec
                    adca      0,X
                    staa      0,X
MANDM2              inx
                    cpx       #DAMENG+9
                    bne       MANDM1
                    tst       DAMSHL
                    beq       MANDM3
                    clr       SHIELD
MANDM3              tst       DAMCOM
                    beq       MANDM4
                    clr       AUTOSR
                    clr       AUTOLR
MANDM4              rts

;
; REPORT DAMAGE HAS OCCURED
;
RPTDAM              ldx       #DMGDST
                    jsr       PSTRNG
RPTDM8              rts

;
; GENERATE A DAMAGE REPORT
;
DAMRPT              ldx       #DMRPST
                    jsr       PSTRNG
                    ldx       #DEVSTR
                    stx       TEMP2
                    ldx       #DAMENG
DMRPT2              tst       0,X
                    bne       DMRPT3
BMPX4               stx       TEMP3
                    ldx       TEMP2
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    inx
                    stx       TEMP2
                    ldx       TEMP3
                    bra       DMRPT4

;
DMRPT3              stx       TEMP3
                    ldx       TEMP2
                    jsr       PSTRNG
                    inx
                    stx       TEMP2
                    ldx       TEMP3
                    ldab      #3
OUTS4               jsr       OUTS
                    decb
                    bne       OUTS4
                    ldaa      0,X
                    jsr       OUTK0
DMRPT4              inx
                    cpx       #DAMENG+9
                    bne       DMRPT2
                    bra       RPTDM8

;
; COMPUTER CODE
;
COMPTR              tst       DAMCOM
                    beq       CMPTR1
                    bsr       RPTDAM
                    rts

;
CMPTR1              ldx       #CPRMPT
                    jsr       PSTRNG
                    jsr       INCH
                    cmpa      #'T'
                    beq       TSPRED
                    cmpa      #'M'
                    beq       CMPMAP
                    cmpa      #'S'
                    bne       CMPTR1
                    ldx       #SRMODE
                    jsr       PSTRNG
                    clr       AUTOSR
                    jsr       INCH
                    cmpa      #'Y'
                    bne       AUTO2
                    inc       AUTOSR
AUTO2               ldx       #LRMODE
                    jsr       PSTRNG
                    clr       AUTOLR
                    jsr       INCH
                    cmpa      #'Y'
                    bne       AUTOEX
                    inc       AUTOLR
AUTOEX              rts

;
CMPMAP              ldx       #CMPHST
                    jsr       PSTRNG
                    ldx       #COMMAP
CMPMP1              jsr       PCRLF
                    ldaa      #8
                    staa      COUNT
CMPMP2              ldaa      0,X
                    cmpa      #$FF
                    bne       CMPMP3
                    stx       TEMP3
                    ldx       #NOSCAN
                    jsr       PDATA1
                    ldx       TEMP3
                    inx
                    bra       CMPMP4

;
CMPMP3              jsr       OUTQIN
CMPMP4              dec       COUNT
                    bne       CMPMP2
                    cpx       #COMMAP+64
                    bne       CMPMP1
                    rts

;
TSPRED              tst       DAMPHT
                    beq       TS2
                    jsr       RPTDAM
                    rts

;
TS2                 clr       PTZFLG
                    ldx       #HWMANY
                    jsr       PSTRNG
                    jsr       INCH
                    cmpa      #'0'
                    blo       TS2
                    beq       TSEX
                    cmpa      #'9'
                    bgt       TS2
                    anda      #$0F
                    staa      PCOUNT
TS3                 jsr       PTRNDM
                    dec       PCOUNT
                    beq       TSEX
                    tst       PTZFLG
                    beq       TS3
TSEX                rts

;
; TRACTOR BEAM ROUTINE
;
TRCTBM              tst       SHUTCR
                    bne       NPCKUP
                    ldaa      SHUTLX
                    cmpa      CURQUX
                    bne       NPCKUP
                    ldaa      SHUTLY
                    cmpa      CURQUY
                    bne       NPCKUP
                    ldx       #SCONBD
                    inc       SHUTCR
                    bra       TRCTEX

;
NPCKUP              ldx       #NOPICK
TRCTEX              jsr       PSTRNG
                    rts

;
; TEXT STRINGS
;
TITLE               fcb       $A
                    fcc       '- - - -   S T A R   T R E K   '
                    fcc       '- - - -      VERSION 1.2'
                    fcb       4
SHTLNG              fcc       'SHORT OR LONG GAME? (S-L): '
                    fcb       4
UPSCAS              fcc       ' UP'
                    fcb       4
BASINF              fcc       'STARBASE IN QUADRANT: '
                    fcb       4
DOCKED              fcc       'DOCKED'
                    fcb       4
DNSCAS              fcc       ' DOWN'
                    fcb       4
PTEMST              fcc       'ALL TORPEDOS FIRED!'
                    fcb       4
INTRO1              fcb       $A
                    fcc       'IT IS STARDATE '
                    fcb       4
INTRO2              fcc       'THE KLINGONS NUMBER '
                    fcb       4
INTRO3              fcc       'YOUR TIME LIMIT '
                    fcc       '(IN STARDATES) = '
                    fcb       4
INTRO4              fcc       'YOU ARE IN QUADRANT '
                    fcb       4
INTRO6              fcc       'AND SECTOR '
                    fcb       4
COMST               fcb       $A
                    fcc       'COMMAND? '
                    fcb       4
DWNST               fcc       'THE SHIELDS ARE DOWN, SIR'
                    fcb       4
UPSTR               fcc       'THE SHIELDS ARE UP, SIR'
                    fcb       4
CRSSTR              fcc       'WHAT COURSE, SIR? (0-7): '
                    fcb       4
WRPSTR              fcc       'WHAT WARP FACTOR, SIR: '
                    fcb       4
BLOKST              fcc       'THE ENTERPRISE IS BLOCKED AT '
                    fcc       'SECTOR '
                    fcb       4
KRMSTR              fcc       `WE'VE RAMMED A KLINGON AT SECTOR `
                    fcb       4
GLBNDS              fcc       `YOU'VE REACHED THE EDGE OF `
                    fcc       'THE GALAXY AND UNKNOWN'
                    fcb       $D,$A
                    fcc       'FORCES HAVE STOPPED THE ENTERPRISE!'
                    fcb       4
PHITST              fcc       'YOUR TORPEDO HAS HIT A '
                    fcb       4
PNOENG              fcc       'THE TORPEDO HAS RUN OUT OF '
                    fcc       'ENERGY'
                    fcb       4
SDATE               fcc       '  STARDATE: '
                    fcb       4
CNDTNS              fcc       '  CONDITION: '
                    fcb       4
YELLOW              fcc       'YELLOW!'
                    fcb       4
RED                 fcc       'RED!!!'
                    fcb       4
GREEN               fcc       'GREEN'
                    fcb       4
QUADP               fcc       '  QUADRANT: '
                    fcb       4
SECP                fcc       '  SECTOR:   '
                    fcb       4
ENGSTR              fcc       '  ENERGY: '
                    fcb       4
KLSTR               fcc       '  KLINGONS: '
                    fcb       4
SHSTR               fcc       '  SHIELDS: '
                    fcb       4
TRPSTR              fcc       '  TORPEDOS: '
                    fcb       4
LRSCST              fcc       'SCAN FOR QUADRANT '
                    fcb       4
MLSHLD              fcc       'YOU MUST FIRST LOWER SHIELDS!'
                    fcb       4
ENAVLB              fcc       'THE AVAILABLE ENERGY IS: '
                    fcb       4
FIRENG              fcc       'HOW MUCH ENERGY SHALL I USE: '
                    fcb       4
TOMUCH              fcc       `YOU DON'T HAVE ENOUGH ENERGY!`
                    fcb       4
PHAMIS              fcc       'PHASORS HAVE MISFIRED!'
                    fcb       4
ALKILL              fcc       'ALL KLINGONS IN THIS SECTOR '
                    fcc       'HAVE BEEN DESTROYED!'
                    fcb       4
KHTADM              fcc       'THE ENEMY HAS BEEN DAMAGED'
                    fcb       4
ERR                 fcc       ' INVALID ENTRY! '
                    fcb       4
STARST              fcc       'STAR, BILLIONS HAVE DIED'
                    fcb       4
BASEST              fcc       'BASE, YOUR ONLY SOURCE OF '
                    fcc       'SUPPLY IS NOW GONE!'
                    fcb       4
KLGSTR              fcc       'KLINGON, CONGRATULATIONS'
                    fcb       4
INTRO0              fcc       'ENTER YOUR 3 LETTER ABORT '
                    fcc       'PASSWORD: '
                    fcb       4
STILFT              fcc       'THE REMAINING KLINGONS NUMBER '
                    fcb       4
KATKDN              fcc       ' UNIT HIT ON THE ENTERPRISE'
                    fcb       4
KATKUP              fcc       'KLINGONS HAVE ATTACKED, '
                    fcc       'BUT THE SHIELDS ARE HOLDING!'
                    fcb       4
DISINT              fcc       'ENTERPRISE HAS BEEN DESTROYED '
                    fcc       '- ALL HANDS LOST!'
                    fcb       4
ABORT1              fcc       'ABORT SEQUENCE STARTED - '
                    fcc       'WHAT WAS YOUR PASSWORD? '
                    fcb       4
ABORT2              fcc       'ABORT SEQUENCE ABANDONED - '
                    fcc       'PASSWORD ERROR'
                    fcb       4
CANTUS              fcc       'THE TELEPORTER REPAIRS ARE '
                    fcc       'NOT YET FINISHED, SORRY'
                    fcb       4
SOMWHR              fcc       'TELEPORTER MALFUNCTION - '
                    fcc       'BASE NOT REACHED'
                    fcb       4
SPSTRM              fcb       7
                    fcc       `WE'VE HIT A SPACE STORM - SHIELDS `
                    fcc       'DAMAGED!'
                    fcb       4
GALDUM              fcc       'THE 3RD ATTEMPT TO LEAVE THE '
                    fcc       'GALAXY HAS CAUSED AN'
                    fcb       $D,$A
                    fcc       'AUTOMATIC SELF-DESTRUCT SEQUENCE. '
                    fcc       'IT CANNOT BE STOPPED!!!'
                    fcb       4
NMENGS              fcb       7
                    fcc       'THE ENTERPRISE IS OUT OF ENERGY. '
                    fcc       'IT CAN NO LONGER EXIST.'
                    fcb       4
NMTMST              fcb       7
                    fcc       'YOU HAVE RUN OUT OF STARDATES '
                    fcc       'FOR THIS MISSION.'
                    fcb       4
NMKLST              fcb       $A,7
                    fcc       'CONGRATULATIONS!'
                    fcb       $A,$D,0,0,0,0
                    fcc       'YOU HAVE DESTROYED ALL THE KLINGONS.'
                    fcb       4
FAILST              fcc       'YOUR MISSION WAS A FAILURE, '
                    fcc       'THE FEDERATION MUST SURRENDER.'
                    fcb       4
SUCCST              fcc       'THE FEDERATION HAS BEEN SAVED '
                    fcc       'BY YOUR GALLANT ACTIONS.'
                    fcb       $D,$A
                    fcc       'YOU ARE AWARDED THE STARFLEET '
                    fcc       'MEDAL OF HONOR.'
                    fcb       4
SUPDES              fcc       'MESSAGE FROM STARFLEET COMMAND:'
                    fcb       $D,$A
                    fcc       '     THE ENTERPRISE HAS JUST BEEN '
                    fcc       'DESTROYED BY A'
                    fcb       $D,$A
                    fcc       `     SUPERNOVA IN IT'S CURRENT `
                    fcc       'QUADRANT - ALL HANDS LOST.'
                    fcb       4
SUPSTR              fcb       7
                    fcc       'SENSORS REPORT A SUPER NOVA IN '
                    fcc       'QUADRANT '
                    fcb       4
HEVDAM              fcc       'BADLY DAMAGED'
                    fcb       4
DMGDST              fcc       'DEVICE IS DAMAGED AND '
                    fcc       'UNUSABLE.  REPAIRS HAVE '
                    fcc       'BEEN STARTED'
                    fcb       4
DMRPST              fcb       $A
                    fcc       'DEVICE     STATUS'
                    fcb       4
DEVSTR              fcc       'ENGINES   '
                    fcb       4
                    fcc       'SHORT SCAN'
                    fcb       4
                    fcc       'LONG SCAN '
                    fcb       4
                    fcc       'PHASORS   '
                    fcb       4
                    fcc       'TORPEDOS  '
                    fcb       4
                    fcc       'SHIELDS   '
                    fcb       4
                    fcc       'TELEPORTER'
                    fcb       4
                    fcc       'TRACTOR BM'
                    fcb       4
                    fcc       'COMPUTER  '
                    fcb       4
PLAYAG              fcc       'WOULD YOU LIKE TO PLA'
                    fcc       'AGAIN? (Y-N): '
                    fcb       4
SCONBD              fcc       'THE SHUTTLE CRAFT IS '
                    fcc       'REPORTED ON BOARD, SIR'
                    fcb       4
NOPICK              fcc       'THE SENSORS SHOW NOTHING '
                    fcc       'TO BE PICKED UP, SIR'
                    fcb       4
EXPCMD              fcc       'THE COMMANDS ARE AS FOLLOWS:'
                    fcb       $D,$A,$A
                    fcc       'CMND   ACTION'
                    fcb       $D,$A
                    fcc       ' EN    ACTIVATE WARP ENGINGS'
                    fcb       $D,$A
                    fcc       ' SR    SHORT RANGE SCAN'
                    fcb       $D,$A
                    fcc       ' LR    LONG RANGE SCAN'
                    fcb       $D,$A
                    fcc       ' PH    FIRE PHASOR BEAMS'
                    fcb       $D,$A
                    fcc       ' PT    FIRE PHOTON TORPEDOS'
                    fcb       $D,$A
                    fcc       ' DR    DAMAGE REPORT'
                    fcb       $D,$A
                    fcc       ' SH    SHIELDS UP OR DOWN'
                    fcb       $D,$A
                    fcc       ' TP    TELEPORT TO BASE QUADRANT'
                    fcb       $D,$A
                    fcc       ' SD    SELF DESTRUCT SEQUENCE'
                    fcb       $D,$A
                    fcc       ' TB    ACTIVATE TRACTOR BEAMS'
                    fcb       $D,$A
                    fcc       ' CO    BATTLE COMPUTER'
                    fcb       $D,$A
                    fcb       4
SHTSIG              fcc       'SENSORS REPORT SHUTTLE '
                    fcc       'CRAFT GALILLEO IN THIS '
                    fcc       'QUADRANT, SIR'
                    fcb       4
SCBKUP              fcc       'SHUTTLE CRAFT SENSORS '
                    fcc       'PROVIDING BACKUP SCAN, SIR'
                    fcb       4
CPRMPT              fcc       'COMPUTER IS CAPABLE OF '
                    fcc       '3 FUNCTIONS:'
                    fcb       $D,$A
                    fcc       '   T = TORPEDO SPREAD'
                    fcb       $D,$A
                    fcc       '   M = PRINT SCAN HISTORY MAP'
                    fcb       $D,$A
                    fcc       '   S = SET AUTO SCAN'
                    fcb       $D,$A,$A
                    fcc       'ENTER COMPUTER COMMAND: '
                    fcb       4
CMPHST              fcc       'STATUS OF QUADRANTS WHEN '
                    fcc       'LAST SCANNED (**** = NO SCAN YET)'
                    fcb       $D,$A,4
NOSCAN              fcc       '**** '
                    fcb       4
HWMANY              fcc       'ENTER NUMBER OF TORPEDOS IN '
                    fcc       'SPREAD (0-9): '
                    fcb       4
SRMODE              fcc       'AUTO SCAN ON FOR SHORT '
                    fcc       'RANGE SENSORS? (Y-N): '
                    fcb       4
LRMODE              fcc       'AUTO SCAN ON FOR LONG '
                    fcc       'RANGE SENSORS? (Y-N): '
                    fcb       4
