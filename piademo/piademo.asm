;******************************
; Demo of PIA input and output
; using switches and a speaker
;
; Hardware: v1 speaker board
;           MP-L in Slot 7
;
; Connections: J1 1-8 = MP-L PA0-PA7
;              J1 9 = GND
;              J1 10 = 5v
;
; Adapted from the demo in the
; KIM-1 manual.
;
;******************************
                    org       $0100
PIABFA              equ       $801C               ; # PIA IN SLOT 7
PIACRA              equ       $801D
PIAINIT             ldaa      PIACRA
                    anda      #%11111011          ; # Enable DDR access
                    staa      PIACRA
                    ldaa      #%00000001          ; # PA7-1 Input, PA0 Output
                    staa      PIABFA
                    oraa      #%00000100          ; # Disable DDR access
                    staa      PIACRA
START               inc       PIABFA
                    ldab      PIABFA
                    comb
                    lsrb
DELAY               decb
                    bpl       DELAY
                    bmi       START
                    end
