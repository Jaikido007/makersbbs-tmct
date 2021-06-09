       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-MESSAGE-TITLE.
       DATA DIVISION.
      
           LINKAGE SECTION.
           01 LS-MESSAGE-NUM UNSIGNED-INT.
           01 LS-MSGS.
               05 LS-MSG OCCURS 100 TIMES
               ASCENDING KEY IS LS-TITLE
               INDEXED BY MSG-IDX.
                   10 LS-TITLE PIC X(60).
                   10 LS-BODY PIC X(500).
                   10 LS-DATE PIC X(10).
                   10 LS-AUTHOR PIC X(10).
           01 TITLE PIC X(60).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-MESSAGE-NUM LS-MSGS 
       RETURNING TITLE.
           
           IF LS-MESSAGE-NUM = 1
                       MOVE LS-TITLE(OFFSET) TO TITLE        
           ELSE IF LS-MESSAGE-NUM = 2
                       MOVE LS-TITLE(OFFSET - 1) TO TITLE
           ELSE IF LS-MESSAGE-NUM = 3
                       MOVE LS-TITLE(OFFSET - 2) TO TITLE 
           ELSE IF LS-MESSAGE-NUM = 4
                       MOVE LS-TITLE(OFFSET - 3) TO TITLE 
           ELSE IF LS-MESSAGE-NUM = 5
                       MOVE LS-TITLE(OFFSET - 4) TO TITLE
           ELSE IF LS-MESSAGE-NUM = 6
                       MOVE LS-TITLE(OFFSET - 5) TO TITLE 
           ELSE IF LS-MESSAGE-NUM = 7
                       MOVE LS-TITLE(OFFSET - 6) TO TITLE
           ELSE IF LS-MESSAGE-NUM = 8
                       MOVE LS-TITLE(OFFSET - 7) TO TITLE
           ELSE IF LS-MESSAGE-NUM = 9
                       MOVE LS-TITLE(OFFSET - 8) TO TITLE
           ELSE IF LS-MESSAGE-NUM = 10
                       MOVE LS-TITLE(OFFSET - 9) TO TITLE                       
           END-IF.

           END FUNCTION DISPLAY-MESSAGE-TITLE.
           
           