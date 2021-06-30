       IDENTIFICATION DIVISION.
       PROGRAM-ID. colour-write.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-COLOUR-FILE ASSIGN TO "customise-file.dat" 
             ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD F-COLOUR-FILE.

           01 RC-ENTRY.
               05 RC-USERNAME PIC X(16).
              *>  05 RC-USER-LEVEL PIC X(3).
               05 RC-STANDARD PIC 9.
               05 RC-TOMATO PIC 9.
               05 RC-UNIX PIC 9.
               05 RC-PAPER PIC 9.
               05 RC-USER-BG PIC 9.
               05 RC-USER-FG PIC 9.
               05 FILLER PIC X VALUE X'0A'.

           WORKING-STORAGE SECTION.

           01 WS-ENTRY.
               05 WS-USERNAME PIC X(16).
              *>  05 RC-USER-LEVEL PIC X(3).
               05 WS-STANDARD PIC 9.
               05 WS-TOMATO PIC 9.
               05 WS-UNIX PIC 9.
               05 WS-PAPER PIC 9.
               05 WS-USER-BG PIC 9.
               05 WS-USER-FG PIC 9.
               05 FILLER PIC X VALUE X'0A'.

           01 WS-END-FILE PIC 9.
           
          *>  01 WS-COLOUR-TABLE.
          *>      05 WS-STANDARD PIC 9.
          *>      05 WS-TOMATO PIC 9.
          *>      05 WS-UNIX PIC 9.
          *>      05 WS-PAPER PIC 9.
           
           LINKAGE SECTION.

           01 LS-USERNAME PIC X(16).
           01 LS-COMMAND PIC X(8).

       PROCEDURE DIVISION USING LS-USERNAME, LS-COMMAND.
           
           MOVE 0 TO WS-END-FILE.
           
           DISPLAY 'USERNAME IS: ' LS-USERNAME.
           DISPLAY 'COMMAND IS: ' LS-COMMAND.

           IF LS-COMMAND = 'new' THEN
               GO 0100-USER-SET-UP
           END-IF.

           IF LS-COMMAND = 'VIP'
             GO 0200-VIP
           END-IF.

           IF LS-COMMAND = 'tomato'
             GO 0300-TOMATO
           END-IF.

           IF LS-COMMAND = 'unix'
             GO 0310-UNIX
           END-IF.

           IF LS-COMMAND = 'paper'
             GO 0320-PAPER
           END-IF.

       0100-USER-SET-UP.
           OPEN EXTEND F-COLOUR-FILE.
           MOVE LS-USERNAME TO RC-USERNAME.
          *>  MOVE LS-ACCOUNT-LEVEL TO RC-USER-LEVEL.
           MOVE 1 TO RC-STANDARD.
           MOVE 0 TO RC-TOMATO.
           MOVE 0 TO RC-UNIX.
           MOVE 0 TO RC-PAPER.
           MOVE 1 TO RC-USER-BG.
           MOVE 7 TO RC-USER-FG.
           WRITE RC-ENTRY.
           CLOSE F-COLOUR-FILE.
           
           GOBACK.
       
       0200-VIP.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE INTO WS-ENTRY
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 DISPLAY 'DEBUG-- USER IS: ' LS-USERNAME
                 DISPLAY 'DEBUG-- RC-USERNAME IS: ' RC-USERNAME
                 IF LS-USERNAME = RC-USERNAME THEN
                 DISPLAY 'DEBUG-- USER FOUND, CONDITION TRUE'
                   MOVE 1 TO RC-TOMATO
                   MOVE 1 TO RC-UNIX
                   MOVE 1 TO RC-PAPER
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GOBACK.

       0300-TOMATO.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE INTO WS-ENTRY
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 DISPLAY 'DEBUG-- USER IS: ' LS-USERNAME
                 DISPLAY 'DEBUG-- RC-USERNAME IS: ' RC-USERNAME
                 IF LS-USERNAME = RC-USERNAME THEN
                 DISPLAY 'DEBUG-- USER FOUND, CONDITION TRUE'
                   MOVE 1 TO RC-TOMATO
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GOBACK.

       0310-UNIX.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE INTO WS-ENTRY
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                *>  DISPLAY 'DEBUG-- USER IS: ' LS-USERNAME
                *>  DISPLAY 'DEBUG-- RC-USERNAME IS: ' RC-USERNAME
                 IF LS-USERNAME = RC-USERNAME THEN
                 DISPLAY 'DEBUG-- USER FOUND, CONDITION TRUE'
                   MOVE 1 TO RC-UNIX
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GOBACK.

       0320-PAPER.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE INTO WS-ENTRY
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 DISPLAY 'DEBUG-- USER IS: ' LS-USERNAME
                 DISPLAY 'DEBUG-- RC-USERNAME IS: ' RC-USERNAME
                 IF LS-USERNAME = RC-USERNAME THEN
                   DISPLAY 'DEBUG-- USER FOUND, CONDITION TRUE'
                   MOVE 1 TO RC-PAPER
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GOBACK.

          *>  | Blue and White = Default 
          *>  | Red and White 
          *>  | Black and Green 
          *>  | White + Black 
          *>  | Current set colours
