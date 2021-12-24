; NAM MITS
; *TO PATCH BASIC TO SWTP
; *WITH ACIA AT PORT 1
; *JAN 11 1978  MICHAEL HOLLEY
; *REV 4   SEPT 4, 1978
;
; *Updated to remove the MSB on OUTCH
; *This removes a graphical glitch.

                    org       $A016
ACIACS              equ       $8004
ACIADA              equ       $8005

; *POLE FOR CHARACTER
; *SETS CARRY IF CHARACTER IS IN BUFFER
; *CLOBBERS B REG
POLCAT              ldab      ACIACS              ; ACIA STATUS TO B
                    asrb      ROTATE              ; RDRF BIT INTO CARRY
                    rts                           ; RETURN

; *INPUT ONE CHARACTER ACC B
INCH                bsr       POLCAT
                    bcc       INCH
                    ldab      ACIADA              ; GET CHAR
                    rts

; *OUTCH OUTPUT CHARACTER ACC A
OUTCH               bsr       POLCAT
                    asrb
                    bcc       OUTCH
                    anda      #%01111111
                    staa      ACIADA              ; OUTPUT
                    rts

; *PATCHES TO MITS BASIC
                    org       $041F
                    jsr       INCH

                    org       $0618
                    jsr       POLCAT

                    org       $08AD
                    jsr       OUTCH

                    end
