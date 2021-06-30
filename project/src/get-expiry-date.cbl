       IDENTIFICATION DIVISION.
       PROGRAM-ID. get-expiry-date.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-USERS-FILE ASSIGN TO 'users.dat'
             ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
           FD F-USERS-FILE.
           01 USERS.
              05 USERNAME PIC X(16).
              05 USER-PASSWORD PIC X(20).
              05 USER-CREDITS PIC 9(3).
              05 USER-LEVEL PIC X(3).
              05 CARD-NO PIC 9(16).
              05 CARD-EXPIRY PIC 9(4).
              05 CARD-CVV PIC 9(3).
              05 FILLER PIC X VALUE X'0A'.

       WORKING-STORAGE SECTION.
           01 FINISHED PIC X VALUE "N".

           01 WS-USERS.
              05 WS-USERNAME PIC X(16).
              05 WS-USER-PASSWORD PIC X(20).
              05 WS-USER-CREDITS PIC 9(3).
              05 WS-USER-LEVEL PIC X(3).
              05 WS-CARD-NO PIC 9(16).
              05 WS-CARD-EXPIRY PIC 9(4).
              05 WS-CARD-CVV PIC 9(3).
              05 FILLER PIC X VALUE X'0A'.
    
       LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-CARD-EXP PIC 9(4). 
       
       PROCEDURE DIVISION USING LS-USERNAME, LS-CARD-EXP.

           MOVE "N" TO FINISHED.
           OPEN INPUT F-USERS-FILE.
           PERFORM UNTIL FINISHED = "Y"
               READ F-USERS-FILE INTO WS-USERS
                 AT END MOVE "Y" TO FINISHED
                 NOT AT END PERFORM GET-CARD-EXP-PROCESS
               END-READ        
           END-PERFORM.    
           CLOSE F-USERS-FILE.
         
           GOBACK.  

       GET-CARD-EXP-PROCESS.
           IF WS-USERNAME = LS-USERNAME THEN
               MOVE WS-CARD-EXPIRY TO LS-CARD-EXP
           END-IF.
