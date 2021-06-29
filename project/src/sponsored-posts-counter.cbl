       IDENTIFICATION DIVISION.
       PROGRAM-ID. sponsored-posts-counter.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-SPONSORED-MESSAGES-FILE ASSIGN TO 
           'sponsored-messages.dat' ORGANIZATION IS LINE SEQUENTIAL.

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
      
           01 WS-CURR-DY PIC X(2).
           01 WS-CURR-MTH PIC X(2).
           01 WS-CURR-YR PIC X(4).

           01 WS-MESSAGE.
               05 WS-DY PIC X(2).
               05 WS-MTH PIC X(2).
               05 WS-YR PIC X(4).
               05 WS-TITLE PIC X(50).
               05 WS-CONTENT PIC X(300).
               05 WS-USERNAME PIC X(16).
  
           01 WS-COUNTER PIC 9.
           01 FINISHED PIC X VALUE "N". 
 
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

           01 LS-SP-COUNTER PIC 9.   
                  
       PROCEDURE DIVISION USING LS-FORMATTED-DT, LS-SP-COUNTER.
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO LS-SP-COUNTER.
           MOVE "N" TO FINISHED. 
           MOVE LS-FORMATTED-DY TO WS-CURR-DY.
           MOVE LS-FORMATTED-MONTH TO WS-CURR-MTH.
           MOVE LS-FORMATTED-YEAR TO WS-CURR-YR.

           OPEN INPUT F-SPONSORED-MESSAGES-FILE.
           PERFORM UNTIL FINISHED = "Y"
               READ F-SPONSORED-MESSAGES-FILE INTO WS-MESSAGE
                 AT END MOVE "Y" TO FINISHED
                 NOT AT END PERFORM COUNTER-PROCESS
               END-READ        
           END-PERFORM.    
           CLOSE F-SPONSORED-MESSAGES-FILE.

           MOVE WS-COUNTER TO LS-SP-COUNTER.
         
           GOBACK.  

       COUNTER-PROCESS.
           IF WS-CURR-DY = WS-DY AND WS-CURR-MTH = WS-MTH THEN
               ADD 1 TO WS-COUNTER  
           END-IF.

           
       
       
       

       
       
