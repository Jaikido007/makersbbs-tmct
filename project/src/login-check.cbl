       IDENTIFICATION DIVISION.
       PROGRAM-ID. login-check.
      ****************************************************************
      *----SUB PROGRAM RESPONSIBLE FOR CHECKING LOGIN CREDENTIALS----*
      ****************************************************************
       DATA DIVISION.
       LINKAGE SECTION.
           01 LS-USERS.
              05 LS-USER OCCURS 100 TIMES
              ASCENDING KEY IS LS-UNAME
              INDEXED BY USER-IDX.
                  10 LS-UNAME PIC X(16).
                  10 LS-PWORD PIC X(20).
           01 LS-CHECK-USERNAME PIC X(16).
           01 LS-CHECK-PASSWORD PIC X(20).  
           01 LS-FOUND PIC 9. 
           01 LS-IDX UNSIGNED-INT.
           01 COUNTER UNSIGNED-INT.
       
       PROCEDURE DIVISION USING LS-USERS LS-CHECK-USERNAME 
           LS-CHECK-PASSWORD LS-FOUND LS-IDX COUNTER.
           MOVE 0 TO LS-FOUND.
           MOVE 1 TO LS-IDX.
           ADD 1 TO COUNTER.
           PERFORM UNTIL LS-IDX = COUNTER
               IF LS-CHECK-USERNAME = LS-UNAME(LS-IDX) AND 
                  LS-CHECK-PASSWORD = LS-PWORD(LS-IDX) THEN
                   MOVE 1 TO LS-FOUND 
               END-IF
               ADD 1 TO LS-IDX
           END-PERFORM.
       