       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'test-add-credits'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-USERNAME PIC X(16).
           01 WS-NEW-USER-NAME PIC X(16).
           01 WS-NEW-PASSWORD PIC X(20).
           01 WS-UPDATE-CREDITS PIC 9(3). 
       PROCEDURE DIVISION.

           MOVE "em" TO WS-USERNAME.
           MOVE 200 TO WS-UPDATE-CREDITS.
          
           CALL 'add-credits' USING WS-USERNAME, WS-UPDATE-CREDITS.
           
           SET WS-UPDATE-CREDITS TO 0.
           MOVE "test" TO WS-USERNAME.
           MOVE 300 TO WS-UPDATE-CREDITS.
          
           CALL 'add-credits' USING WS-USERNAME, WS-UPDATE-CREDITS.

           
           