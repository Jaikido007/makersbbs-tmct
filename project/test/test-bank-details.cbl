       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'test-bank-details'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-USERNAME PIC X(16).
           01 WS-CARD-NO PIC 9(16).
           01 WS-CARD-EXPIRY PIC 9(4).
           01 WS-CARD-CSV PIC 9(3).  
       PROCEDURE DIVISION.

           MOVE "em" TO WS-USERNAME.
           MOVE 9999999999999999 TO WS-CARD-NO.
           MOVE 9999 TO WS-CARD-EXPIRY.
           MOVE 999 TO WS-CARD-CSV.
           
           CALL 'bank-details' USING WS-USERNAME, WS-CARD-NO,
           WS-CARD-EXPIRY, WS-CARD-CSV.
           