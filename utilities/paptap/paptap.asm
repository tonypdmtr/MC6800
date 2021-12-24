; NAM PAPTAP
; HIGH SPEED PAPER TAPE LOADER PROGRAM
; DEVELOPED BY DR. CHARLES ADAMS
; TEXAS A&M UNIVERSITY
;
; MODIFIED BY CHRISTOPHER MALLERY
; - Made it easier to grok
; - Changed PIA routines to match what
;   I understand.
;
                    org       $9000

PIABDR              equ       $801E               ; PIA IN SLOT 7, DATA REGISTER OF PORT B
PIABCR              equ       $801F               ; PIA IN SLOT 7, CONTROL REGISTER OF PORT B

OUTCH               equ       $E1D1               ; OUTPUT CHARACTER ROUTINE

ENTER               bsr       PIASUP              ; INIT PIA
START               bsr       PIADAT              ; GET 1ST BYTE
                    cmpa      #'S'                ; IS IT AN S?
                    bne       START               ; IF NOT, START OVER
                    bsr       PIADAT              ; GET 2ND BYTE
                    cmpa      #$31                ; IS IT A 1?
                    bne       START               ; IF NOT, START OVER
                    clr       CHKSUM
                    bsr       GETBYTE             ; GET THE BYTE COUNT
                    suba      #2
                    staa      NUMBYT
                    bsr       GETADDR             ; GET ADDRESS AND LOAD X

LOOP                bsr       GETBYTE             ; READ THE BYTES
                    dec       NUMBYT
                    beq       CHKCHK              ; LAST BYTE IS CHECKSUM
                    staa      0,X
                    inx
                    bra       LOOP

CHKCHK              inc       CHKSUM              ; CHECK THE CHECKSUM
                    beq       START               ; GO TO NEXT LINE
                    ldaa      #'!'
                    jsr       OUTCH
                    swi                           ; FAILED CHECK SUM, SO INTERRUPT

;*********** PIASUP - PIA SETUP ROUTINE
PIASUP              ldaa      #$32                ; C1 SET ON RISING EDGE OF RDY & ACK=0
                    staa      PIABCR              ; SET CRB
                    clra
                    staa      PIABDR              ; SET DDRB = ALL INPUTS
                    ldaa      #$3E                ; RAISE ACK ON C2 (RESET RDY ON C1)
                    staa      PIABCR              ; SET CRB
                    rts                           ; AND LEAVE...

;*********** PIADAT - GET DATA FROM PIA
PIADAT              ldaa      #$36                ; C1 SET ON RISING EDGE OF RDY & ACK=0
                    staa      PIABCR              ; SET CRB
DATLOP              ldaa      PIABCR              ; GET RDY ON C1
                    bmi       GETDAT              ; DATA PRESENT? (BIT 7 == 1)
                    bra       DATLOP              ; NOT YET. KEEP TRYING.

GETDAT              ldaa      PIABDR              ; YES. GET THE DATA,
                    jsr       OUTCH               ; PRINT IT OUT
                    psha                          ; AND SAVE IT.
                    ldaa      #$3E                ; RAISE ACK ON C2 (RESET RDY ON C1)
                    staa      PIABCR
                    pula                          ; PUT DATA BACK IN REGISTER
                    rts                           ; AND LEAVE...

;*********** GETBYTE - READ ASCII HEX BYTE (2 digits)
GETBYTE             bsr       A2H                 ; GET FIRST CHARACTER TO HEX
                    asla                          ; MOVE IT UP 4 BITS
                    asla
                    asla
                    asla
                    tab
                    bsr       A2H                 ; GET SECOND CHARACTER TO HEX
                    aba                           ; ADD THEM TOGETHER
                    tab
                    addb      CHKSUM              ; ADD TO CHECKSUM
                    stab      CHKSUM
                    rts

;*********** GETADDR - READ ADDRESS AND LOAD X
GETADDR             bsr       GETBYTE
                    staa      TMPADDR1
                    bsr       GETBYTE
                    staa      TMPADDR2
                    ldx       TMPADDR1
                    rts

;*********** A2H - READ SINGLE ASCII BYTE AND CONVERT TO HEX
A2H                 bsr       PIADAT
                    suba      #$30
                    cmpa      #09
                    ble       RT
                    suba      #7
RT                  rts

NUMBYT              rmb       1                   ; BYTES LEFT TO READ
TMPADDR1            rmb       1                   ; TEMP ADDRESS POINTER
TMPADDR2            rmb       1
CHKSUM              rmb       1                   ; RUNNING CHECKSUM

                    end
