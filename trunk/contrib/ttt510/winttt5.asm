;{--------------------------------------------------------------------------}
;{                         TechnoJock's Turbo Toolkit                       }
;{                                                                          }
;{                              Version 5.10                                }
;{                                                                          }
;{                                                                          }
;{             Copyright (c) 1986-1993 TechnoJock Software Inc.             }
;{                     All Rights Reserved - Worldwide                      }
;{--------------------------------------------------------------------------}
;
;                     {-------------------------------}                                       
;                     {       OBJ:   WinTTT5          }
;                     {-------------------------------}
;
;

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+                                                                + 
;+          This asm file contains 2 pascal procedures for        +  
;+          linking with TTT version 5.0. The procedures are:     +
;+                                                                +
;+           MoveFromScreen(var Source, Dest; Length : Word);     +
;+           MoveToScreen(var Source, Dest; Length : Word);       +
;+                                                                + 
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


DATA    SEGMENT BYTE PUBLIC

        EXTRN   SnowProne : BYTE

DATA    ENDS


CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  MoveFromScreen, MoveToScreen

;+++++++++++++++++++++++++++++++++++++++
;+     M O V E F R O M S C R E E N     +
;+++++++++++++++++++++++++++++++++++++++

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+                                                                +
;+    Move data from screen to DEST without snow. Note that       +
;+    the length is in WORDS not bytes.                           +
;+                                                                +
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MFLength        EQU     WORD PTR [BP+6]
MFDest          EQU     DWORD PTR [BP+8]
MFSource        EQU     DWORD PTR [BP+12]

MoveFromScreen  PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        MOV     BX,DS                   ;Save DS in BX
        MOV     AL,Snowprone            ;Grab before changing DS
        LES     DI,MFDest               ;ES:DI points to Dest
        LDS     SI,MFSource             ;DS:SI points to Source
        MOV     CX,MFLength             ;CX = Length
        CLD                             ;Set direction to forward
        RCR     AL,1                    ;Check WaitForRetrace
        JNC     MFNoWait                ;False? Use MFNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
MFNext:
        CLI                             ;No interrupts now
MFWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vertical retrace
        JNZ     MFGo                    ;In progress? go
        RCR     AL,1                    ;Wait for end of horizontal
        JC      MFWaitNoH               ; retrace
MFWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     MFWaitH                 ; retrace
MFGo:
        LODSW                           ;Load next video word into AX
        STI                             ;Allow interrupts
        STOSW                           ;Store video word in Dest
        LOOP    MFNext                  ;Get next video word
        JMP     MFExit                  ;All Done
MFNoWait:
        REP     MOVSW                   ;That's it!
MFExit:
        MOV     DS,BX                   ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     10                      ;Remove parameters and return

MoveFromScreen  ENDP

;+++++++++++++++++++++++++++++++++++
;+     M O V E T O S C R E E N     +
;+++++++++++++++++++++++++++++++++++

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+                                                                +
;+    Move data from SOURCE to DEST without snow. Note that       +
;+    the length is in WORDS not bytes.                           +
;+                                                                +
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;****************************************************** MoveToScreen

;procedure MoveToScreen(var Source, Dest; Length : Word);
;Move Length words from Source to Dest (video memory) without snow

;equates for parameters:
MTLength        EQU     WORD PTR [BP+6]
MTDest          EQU     DWORD PTR [BP+8]
MTSource        EQU     DWORD PTR [BP+12]

MoveToScreen    PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        PUSH    DS                      ;Save DS
        MOV     AL,SnowProne            ;Grab before changing DS
        LES     DI,MTDest               ;ES:DI points to Dest
        LDS     SI,MTSource             ;DS:SI points to Source
        MOV     CX,MTLength             ;CX = Length
        CLD                             ;Set direction to forward
        RCR     AL,1                    ;Check WaitForRetrace
        JNC     MTNoWait                ;False? Use MTNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
MTGetNext:
        LODSW                           ;Load next video word into AX
        MOV     BX,AX                   ;Store video word in BX
        CLI                             ;No interrupts now
MTWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vertical retrace
        JNZ     MTGo                    ;In progress? Go
        RCR     AL,1                    ;Wait for end of horizontal
        JC      MTWaitNoH               ; retrace
MTWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     MTWaitH                 ; retrace
MTGo:
        MOV     AX,BX                   ;Move word back to AX...
        STOSW                           ; and then to screen
        STI                             ;Allow interrupts
        LOOP    MTGetNext               ;Get next video word
        JMP     MTExit                  ;All done
MTNoWait:
        REP     MOVSW                   ;That's all!
MTExit:
        POP     DS                      ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     10                      ;Remove parameters and return

MoveToScreen    ENDP

CODE    ENDS

        END

