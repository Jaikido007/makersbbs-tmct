       IDENTIFICATION DIVISION.
       PROGRAM-ID. sign-up.
      ***************************************************
      *----SUB PROGRAM RESPONSIBLE FOR USER SIGN UPS----*
      ***************************************************
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
           LINKAGE SECTION.
           01 LS-NEW-USER-NAME PIC X(16).
           01 LS-NEW-PASSWORD PIC X(20).
                
           PROCEDURE DIVISION USING LS-NEW-USER-NAME LS-NEW-PASSWORD.

           OPEN EXTEND F-USERS-FILE
               MOVE LS-NEW-USER-NAME TO USERNAME
               MOVE LS-NEW-PASSWORD TO USER-PASSWORD
               WRITE USERS
               END-WRITE
           CLOSE F-USERS-FILE.
       