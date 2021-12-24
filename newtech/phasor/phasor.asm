; NAM     PHASOR  REV.A  SEPTEMBER 1977
; OPT     NOP,O
; COPYRIGHT (C) ALL RIGHTS RESERVED
; NEWTECH COMPUTER SYSTEMS,INC.
;

                    org       $0100
START               ldaa      SWEEPN              ; INIT. SWEEP COUNT
                    staa      SWEEPS
NEXTS               ldaa      #0                  ; EXIT IF SWEEPS COUNT0
                    cmpa      SWEEPS              ; ELSE DECREMENT COUNT &
                    beq       EXIT                ; DO ONE SWEEP
                    dec       SWEEPS
                    ldaa      FIRSTF              ; INIT.FREQ.PARAMETER.
                    staa      FREQ
                    ldaa      RATE                ; GET SWEEP RATE PARAM.
LOOP3               deca                          ; DECREMENT IT.
                    bne       LOOP1               ; IF N.E.0 BRANCH.
                    inc       FREQ                ; ELSE DECREASE FREQ.
                    ldaa      LASTF               ; IF LOWEST FREQUENCY
                    cmpa      FREQ                ; THEN SWEEP IS DONE SO
                    beq       NEXTS               ; GO NEXT SWEEP.
                    ldaa      RATE                ; ELSE RESTORE RATE PARAM.
                    bra       LOOP2

LOOP1               com       0,X                 ; WASTE TIME
                    com       0,X
                    nop
LOOP2               ldab      FREQ                ; HALF-WAVE TIMEOUT
LOOP4               decb
                    bne       LOOP4
                    com       TOGGLE              ; OUTPUT COMPLEMENT TO
                    ldab      TOGGLE              ; MUSIC BOARD.
                    stab      MOD68
                    bra       LOOP3

EXIT                jmp       MIKBUG

MOD68               equ       $8010               ; MUSIC BOARD ADDRESS.
SWEEPN              fcb       $0D                 ; DESIRED # OF SWEEPS.
SWEEPS              rmb       1                   ; TEMPORARY SWEEP COUNT
FIRSTF              fcb       $01                 ; STARTING SWEEP PARAM.
LASTF               fcb       $FF                 ; ENDING SWEEP PARAM.
FREQ                rmb       1                   ; TEMPORARY FREQ.PARAM.
RATE                fcb       $01                 ; SWEEP RATE PARAMETER.
TOGGLE              fcb       0
MIKBUG              equ       $E0D0
                    nop
                    end
