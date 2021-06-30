              IDENTIFICATION DIVISION.
       PROGRAM-ID. colour-set.
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

           01 WS-END-FILE PIC 9.

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
           
          *>  01 WS-COLOUR-TABLE.
          *>      05 WS-STANDARD PIC 9.
          *>      05 WS-TOMATO PIC 9.
          *>      05 WS-UNIX PIC 9.
          *>      05 WS-PAPER PIC 9.
           
           LINKAGE SECTION.

           01 LS-USERNAME PIC X(16).
          
           01 LS-COMMAND PIC X(8).

           01 LS-COLOUR-TABLE.
               05 LS-STANDARD PIC 9.
               05 LS-TOMATO PIC 9.
               05 LS-UNIX PIC 9.
               05 LS-PAPER PIC 9.
               05 LS-USER-BG PIC 9.
               05 LS-USER-FG PIC 9.

       PROCEDURE DIVISION USING LS-USERNAME, LS-COLOUR-TABLE, 
               LS-COMMAND.
           
           MOVE 0 TO WS-END-FILE.

           IF LS-COMMAND = 'standard' THEN
               GO 0100-STANDARD
           END-IF.

           IF LS-COMMAND = 'tomato'
             GO 0110-TOMATO
           END-IF.

           IF LS-COMMAND = 'unix'
             GO 0120-UNIX
           END-IF.

           IF LS-COMMAND = 'paper'
             GO 0130-PAPER
           END-IF.
       
       0100-STANDARD.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 IF LS-USERNAME = RC-USERNAME THEN
                   MOVE 1 TO RC-USER-BG
                   MOVE 7 TO RC-USER-FG
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GO 0200-RETURN-TABLE.

       0110-TOMATO.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 IF LS-USERNAME = RC-USERNAME THEN
                   MOVE 4 TO RC-USER-BG
                   MOVE 7 TO RC-USER-FG
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GO 0200-RETURN-TABLE.

       0120-UNIX.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 IF LS-USERNAME = RC-USERNAME THEN
                   MOVE 0 TO RC-USER-BG
                   MOVE 2 TO RC-USER-FG
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GO 0200-RETURN-TABLE.

       0130-PAPER.
           OPEN I-O F-COLOUR-FILE.
           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 IF LS-USERNAME = RC-USERNAME THEN
                   MOVE 7 TO RC-USER-BG
                   MOVE 0 TO RC-USER-FG
                   REWRITE RC-ENTRY
                   END-REWRITE
                 END-IF
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GO 0200-RETURN-TABLE.

       0200-RETURN-TABLE.
           MOVE 0 TO WS-END-FILE.
           OPEN INPUT F-COLOUR-FILE.

           PERFORM UNTIL WS-END-FILE = 1
             READ F-COLOUR-FILE
               AT END MOVE 1 TO WS-END-FILE
               NOT AT END
                 IF LS-USERNAME = RC-USERNAME THEN 
                   MOVE RC-STANDARD TO LS-STANDARD
                   MOVE RC-TOMATO TO LS-TOMATO
                   MOVE RC-UNIX TO LS-UNIX
                   MOVE RC-PAPER TO LS-PAPER
                   MOVE RC-USER-BG TO LS-USER-BG
                   MOVE RC-USER-FG TO LS-USER-FG
                 END-IF 
           END-PERFORM.
           CLOSE F-COLOUR-FILE.
           GOBACK.  
