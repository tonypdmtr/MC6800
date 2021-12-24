; NAM     PLAY68          REV.B MAY 1978
; OPT     O,NOP
; COPYRIGHT (C) 1978 ALL RIGHTS RESERVED.
; NEWTECH COMPUTER SYSTEMS,INC.
;
; PLAY68 STARTS AT THE BEGINNING OF THE MEMORY AREA
; DESIGNATED "SCORE" AND TRANSFERS INTO RAM LOCATION
; "PITCH" A 1-BYTE PITCH PARAMETER AND INTO RAM
; LOCATION "DURA" A 2-BYTE DURATION PARAMETER.
; THE ROUTINE THEN OUTPUTS TO
; THE MODEL 68 THE MUSICAL NOTE SPECIFIED BY THESE
; NOTE PARAMETERS.  PLAY68 CONTINUES TRANSFERRING
; NOTE PARAMETERS AND OUTPUTING EACH NOTE UNTIL
; A PITCH CONSTANT OF ZERO IS ENCOUNTERED WHICH
; INDICATES THE END OF THE MUSICAL SCORE.
; THIS ROUTINE DOES NOT USE THE STACK.
;
BASEND              equ       $1EE2               ; END OF SWTPC 8K BASIC 2.2
                    org       BASEND+$20
SCORE               equ       BASEND+$0120        ; SCORE LOCATION
                    ldx       #SCORE              ; INIT. SCORE POINTER.
                    stx       PLACE
                    bra       NEXT

EXIT1               rts

                    nop
                    nop
NEXT                ldx       PLACE
                    ldaa      #0
                    cmpa      0,X
HERE                beq       EXIT1               ; YOUR ENDING?
;   ELSE TRANSFER PARAMTERS FOR NEXT NOTE OR SCORE
;   INTO PLAY ROUTINE
                    ldaa      0,X                 ; LOAD PITCH
                    staa      PITCH
                    inx
                    ldaa      0,X                 ; LOAD DURATION MSB
                    anda      #%00000111          ; MASK 3 LSB'S
                    staa      DURA
                    ldaa      0,X                 ; GET MSB AGAIN
                    anda      #%01110000          ; MASK 3 BITS
                    adda      #TBL1               ; ENVELOPE SPEC ADDRESS
                    staa      PLAY+2              ; LOAD ENVELOPE POINTER
                    inx
                    ldaa      0,X                 ; LOAD DURATION LSB.
                    staa      DURA+1
                    inx
                    stx       PLACE               ; SAVE SCORE POINTER
;    THE PLAY ROUTING PLAYS ONE NOTE
PLAY                ldx       #TBL1               ; INIT EVELOPE POINTER.
                    stx       TBL1P               ; STORE ENVELOPE POINTER.
                    ldab      0,X                 ; PUT AMPLITUDE VALUE IN B.
                    ldx       DURA                ; LOAD DURATION PARAMETER
;                                INTO INDEX REGISTER.
LOOP3               cpx       $E000               ; 5-WASTE TIME (31 STATES)
                    cpx       $E000               ; 5-
                    cpx       $E000               ; 5-
                    cpx       $E000               ; 5-
                    cpx       $E000               ; 5-
                    com       $E000               ; 6-
LOOP2               ldaa      #22                 ; 4-FIXED DELAY TO ADJUST
LOOP4               deca                          ; 2- LOWEST NOTE TO 262HZ
                    bne       LOOP4               ; 4- (MIDDLE c) WHEN PITCH
;                                  PARAMETER=FE
                    ldaa      PITCH               ; 4-LOAD PITCH PARAMETER
                    stab      MOD68               ; 5-OUTPUT TO MUSIC BOARD
LOOP1               deca                          ; 2-DELAY AS PER PITCH PARAM.
                    bne       LOOP1               ; 4-
                    comb                          ; 2-COMPLEMENT WAVEFORM VALUE.
                    dex                           ; 4-DECREMENT DURATION COUNTER.
                    bne       LOOP3               ; 4-
                    inc       TBL1P+1             ; 6-SET UP NEXT SEGMENT
                    ldx       TBL1P               ; 5-
                    ldab      0,X                 ; 5-
                    cmpb      #$01                ; 2-END OF ENVELOPE CHAR.=01
                    beq       NEXT                ; 4-GO DO NEXT NOTE.
                    ldx       DURA                ; 5-RESET DURATION PARAMETER
                    bra       LOOP2               ; 4-

;
; AMPLITUDE ENVELOPE SPECIFICATION:
; MAXIMUM APLITUDE IS OUTPUT WHEN ACCUMULATOR B IS
; COMPLEMENTED FROM 00 TO FF AND BACK.  MINIMUM
; AMPLITUDE IS OUTPUT WHEN B IS COMPLEMENTED
; BETWEEN 80 AND 7F.  AN END OF ENVELOPE RECORD
; OF $01 MARKS THE END OF THE SPECIFICATION.
;
TBL1                equ       *                   ; ENVELOPE SPECIFICATIONS
; TABLE 1 - ATTACK, #5
                    fcb       $FF,$FF,$F8,$F0,$E8,$E0,$D8,$D0
                    fcb       $C8,$C0,$B8,$B0,$A0,$90,$85,$01
; TABLE 2 - REST, #R
                    fcb       $80,$80,$80,$80,$80,$80,$80,$80
                    fcb       $80,$80,$80,$80,$80,$80,$80,$01
; TABLE 3 - STACCATO, #S
                    fcb       $E0,$F0,$FF,$E5,$C8,$BD,$B0,$A5
                    fcb       $98,$8D,$80,$80,$80,$80,$80,$01
; TABLE 4 - LEGATO, #L
                    fcb       $E0,$F0,$FF,$FF,$FF,$FF,$FF,$FF
                    fcb       $FF,$FF,$FF,$E8,$D0,$CD,$C8,$01
; TABLE 5 - SOFT STACCATO, #1
                    fcb       $D8,$BD,$C0,$B8,$B0,$A0,$90,$88
                    fcb       $80,$80,$80,$80,$80,$80,$80,$01
; TABLE 6 - SOFT LEGATO, #2
                    fcb       $B8,$BD,$C0,$C0,$C0,$C0,$C0,$C0
                    fcb       $C0,$C0,$C0,$B8,$B0,$A8,$A0,$01
; TABLE 7 - "SHAPED", #3
                    fcb       $D0,$D6,$DD,$E3,$E8,$F5,$FF,$FF
                    fcb       $FF,$FF,$FF,$F3,$E5,$DA,$D0,$01
; TABLE 8 - CRESCENDO, #4
                    fcb       $B8,$BE,$C4,$CA,$D0,$D6,$DC,$E2
                    fcb       $E8,$F4,$FF,$FF,$FF,$CD,$A0,$01
;
DURA                rmb       2                   ; DURATION CONSTANT
MOD68               equ       $8010               ; MUSIC BOARD IN I/0 SLOT 4.
PLACE               rmb       2
TBL1P               rmb       2                   ; TABLE POINTER.
PITCH               rmb       1                   ; PITCH PARAMETER.

                    org       BASEND
; KEYTONE ROUTINE
; THE FOLLOWING ROUTINE PROVIDES A SHORT TONE
; OR "BEEP" EACH TIME A KEY IS STRUCK ON THE
; KEYBOARD AT PORT 1.  THIS IS THE PORT THAT
; BASIC NORMALLY USES AS ITS CONTROL PORT.
BEEP                fcb       $20,$28,$08,$00     ; "BEEP" SCORE
INEEE               equ       $E1AC               ; MIKBUG/SWTBUG INPUT
PRMFLG              equ       $85                 ; BASIC PROMPT FLAG
KEYTON              jsr       INEEE               ; GET INPUT CHARACTER
                    psha                          ; SAVE CHARACTER
                    ldaa      PRMFLG              ; PROMPT REQUIRED?
                    bne       ENDTON              ; GO IF NOT
                    ldx       #BEEP               ; POINT TO SCORE
                    stx       PLACE
                    bsr       NEXT                ; PLAY BEEP TONE
ENDTON              pula                          ; RESTORE CHARACTER
                    rts

; FORCE PORT 1 INPUT TO GO TO KEYTON ROUTINE
                    org       $112                ; PORT 1 INPUT JUMP
                    jmp       KEYTON              ; INPUT VECTOR

; FORCE BASIC TO LEAVE PLAY68 MEMORY UNUSED
                    org       $14E                ; BASIC WORK SPACE POINTER
                    fdb       BASEND+$400         ; RESERVE 1K

; PLUG MIKBIG/SWTBUG PROGRAM COUNTER TO CAUSE
; ENTRY INTO SWTPC 8K BASIC 2.2.
                    org       $A048               ; PROGRAM COUNTER
                    fdb       $100                ; BASIC "COLD ENTRY POINT
                    end
