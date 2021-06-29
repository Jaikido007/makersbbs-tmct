       IDENTIFICATION DIVISION.
       PROGRAM-ID. post-sponsored-message.
      ***************************************************
      *----SUB PROGRAM RESPONSIBLE FOR USER SIGN UPS----*
      ***************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-SPONSORED-MESSAGES-FILE ASSIGN TO 
           'sponsored-messages.dat'
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD F-SPONSORED-MESSAGES-FILE.
           01 SP-MESSAGE.
               05 SP-DY PIC X(2).
               05 SP-MTH PIC X(2).
               05 SP-YR PIC X(4).
               05 SP-TITLE PIC X(50).
               05 SP-CONTENT PIC X(300).
               05 SP-USERNAME PIC X(16).

           WORKING-STORAGE SECTION.
           01 WS-MESSAGE.
             05 WS-DY PIC X(2).
             05 WS-MTH PIC X(2).
             05 WS-YR PIC X(4).
             05 WS-TITLE PIC X(50).
             05 WS-CONTENT PIC X(300).  
             05 WS-USERNAME PIC X(16).    
 
           LINKAGE SECTION.
           01 LS-FORMATTED-DT.
             05 LS-FORMATTED-DTE-TME.
               15 LS-FORMATTED-YEAR    PIC  X(4). 
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-MONTH   PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-DY      PIC  X(2).
               15 FILLER               PIC X VALUE '-'.
               15 LS-FORMATTED-HOUR    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-MINS    PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-SEC     PIC  X(2).
               15 FILLER               PIC X VALUE ':'.
               15 LS-FORMATTED-MS      PIC  X(2).

           01 LS-MESSAGE.
             05 LS-TITLE PIC X(50).
             05 LS-CONTENT PIC X(300).
             05 LS-MSG-AUTHOR PIC X(16).    
                  
           PROCEDURE DIVISION USING LS-FORMATTED-DT, LS-MESSAGE.
           
           
           MOVE LS-FORMATTED-DY TO WS-DY.
           MOVE LS-FORMATTED-MONTH TO WS-MTH.
           MOVE LS-FORMATTED-YEAR TO WS-YR.    
           MOVE LS-TITLE TO WS-TITLE.
           MOVE FUNCTION TRIM(LS-CONTENT) TO WS-CONTENT.
           MOVE LS-MSG-AUTHOR TO WS-USERNAME.

           OPEN EXTEND F-SPONSORED-MESSAGES-FILE. 
           MOVE WS-MESSAGE TO SP-MESSAGE. 
           WRITE SP-MESSAGE.
           CLOSE F-SPONSORED-MESSAGES-FILE.

       