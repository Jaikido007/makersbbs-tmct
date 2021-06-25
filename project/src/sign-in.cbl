       IDENTIFICATION DIVISION.
       PROGRAM-ID. sign-in.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-USERS-FILE ASSIGN TO 'users.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
       
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
       
       WORKING-STORAGE SECTION.
           01 WS-USERS.
               05 WS-USER OCCURS 100 TIMES
               ASCENDING KEY IS WS-UNAME
               INDEXED BY USER-IDX.
                   10 WS-UNAME PIC X(16).
                   10 WS-PWORD PIC X(20).
           01 WS-FOUND PIC 9.
           01 WS-IDX UNSIGNED-INT.
           01 WS-FILE-IS-ENDED PIC 9.
           01 COUNTER UNSIGNED-INT.
           01 WS-CHECK-USERNAME PIC X(16).
           01 WS-CHECK-PASSWORD PIC X(20).
       LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-PASSWORD PIC X(20).
           01 LS-LOGIN-CORRECT PIC 9.     
       
       PROCEDURE DIVISION USING LS-USERNAME, LS-PASSWORD, 
           LS-LOGIN-CORRECT.

           SET WS-FILE-IS-ENDED TO 0.
           SET WS-IDX TO 0.
           SET COUNTER TO 0.

           OPEN INPUT F-USERS-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       ADD 1 TO COUNTER
                       MOVE USERNAME TO WS-UNAME(COUNTER)
                       MOVE USER-PASSWORD TO WS-PWORD(COUNTER)
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-USERS-FILE.

           MOVE LS-USERNAME TO WS-CHECK-USERNAME.
           MOVE LS-PASSWORD TO WS-CHECK-PASSWORD.

           CALL 'login-check' USING WS-USERS WS-CHECK-USERNAME 
           WS-CHECK-PASSWORD WS-FOUND WS-IDX COUNTER.


           MOVE WS-FOUND TO LS-LOGIN-CORRECT. 
       

       
       