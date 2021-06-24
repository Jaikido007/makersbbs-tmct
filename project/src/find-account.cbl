       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-account.
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
              05 CARD-CSV PIC 9(3).    
       
       WORKING-STORAGE SECTION.
           01 WS-FOUND PIC 9.
           01 WS-IDX UNSIGNED-INT.
           01 WS-FILE-IS-ENDED PIC 9.
           01 COUNTER UNSIGNED-INT.
           01 WS-CHECK-USERNAME PIC X(16).
           01 WS-CHECK-CREDITS PIC 9(3).
       LINKAGE SECTION.
           01 LS-USERNAME PIC X(16).
           01 LS-USERCREDITS PIC 9(3).     
       
       PROCEDURE DIVISION USING LS-USERNAME, LS-USERCREDITS.

           SET WS-FILE-IS-ENDED TO 0.
           SET COUNTER TO 0.

           OPEN INPUT F-USERS-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-USERS-FILE
                   NOT AT END
                       IF USERNAME = LS-USERNAME
                       MOVE USER-CREDITS TO LS-USERCREDITS
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED
               END-READ 
           END-PERFORM.
           CLOSE F-USERS-FILE.
       

       
       