              IDENTIFICATION DIVISION.
       PROGRAM-ID. colour-read.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-COLOUR-FILE ASSIGN TO "customise-file.dat" 
             ORGANIZATION IS LINE SEQUENTIAL.

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
           
           LINKAGE SECTION.

           01 LS-USERNAME PIC X(16).

           01 LS-COLOUR-TABLE.
               05 LS-STANDARD PIC 9.
               05 LS-TOMATO PIC 9.
               05 LS-UNIX PIC 9.
               05 LS-PAPER PIC 9.
               05 LS-USER-BG PIC 9.
               05 LS-USER-FG PIC 9.    

       PROCEDURE DIVISION USING LS-USERNAME, LS-COLOUR-TABLE.
           
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
