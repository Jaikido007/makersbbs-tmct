       IDENTIFICATION DIVISION.
       PROGRAM-ID. list-all-messages.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGES-FILE ASSIGN TO "messages.dat"
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-MESSAGES-FILE.
           01 RC-MESSAGE.
               05 RC-ID PIC 999.
               05 RC-MESSAGE-TITLE PIC X(50).
               05 RC-MESSAGE-CONTENT PIC X(300).
               05 RC-USERNAME PIC X(16).
               05 RC-DATE PIC X(10).

           WORKING-STORAGE SECTION.
           01 NUM-OF-LINES PIC 999.
           01 WS-TABLE.
               05 WS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
                   NUM-OF-LINES.
                   10 WS-ID PIC XXX.
                   10 WS-TITLE PIC X(50).
                   10 WS-CONTENT PIC X(300).
                   10 WS-USERNAME PIC X(16).
                   10 WS-DATE PIC X(10).
           
           01 LOOP-COUNTER PIC 999.
           01 WS-FILE-END PIC 9.

           LINKAGE SECTION.
           01 LS-RETURN-TABLE.
               05 LS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
                 NUM-OF-LINES.
                   10 LS-ID PIC XXX.
                   10 LS-TITLE PIC X(50).
                   10 LS-CONTENT PIC X(300).
                   10 LS-USERNAME PIC X(16).
                   10 LS-DATE PIC X(10).

       PROCEDURE DIVISION USING LS-RETURN-TABLE.

      ******************************************************************
      *******-----MOVE ALL ENTRIES IN FILE TO LOCAL TABLE-----**********
      ******************************************************************
           
           CALL 'number-of-messages' USING NUM-OF-LINES.
           MOVE 0 TO WS-FILE-END.
           
           MOVE 0 TO LOOP-COUNTER.

           OPEN INPUT F-MESSAGES-FILE.

           PERFORM UNTIL WS-FILE-END = 1
             READ F-MESSAGES-FILE
             NOT AT END
               ADD 1 TO LOOP-COUNTER
               MOVE RC-MESSAGE TO WS-ENTRY(LOOP-COUNTER)
               
             AT END MOVE 1 TO WS-FILE-END  
                 
           END-PERFORM.

           CLOSE F-MESSAGES-FILE.
          
      ******************************************************************
      *********-----STORE LOCAL TABLE TO EXPORTED TABLE-----************
      ******************************************************************

           MOVE WS-TABLE TO LS-RETURN-TABLE.

      ******************************************************************
      ********-----FLIP INDEXES OF EXPORTED TABLE ENTRIES-----**********
      ******************************************************************

           CALL 'id-sort' USING LS-RETURN-TABLE.
           
      ******************************************************************
