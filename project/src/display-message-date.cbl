       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-MESSAGE-DATE.
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
           01 POST-DATE PIC X(10).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-MESSAGE-NUM LS-MSGS 
       RETURNING POST-DATE.
           
           IF LS-MESSAGE-NUM = 1
                       MOVE LS-DATE (OFFSET) TO POST-DATE    
           ELSE IF LS-MESSAGE-NUM = 2
                       MOVE LS-DATE (OFFSET - 1) TO POST-DATE
           ELSE IF LS-MESSAGE-NUM = 3
                       MOVE LS-DATE (OFFSET - 2) TO POST-DATE 
           ELSE IF LS-MESSAGE-NUM = 4
                       MOVE LS-DATE (OFFSET - 3) TO POST-DATE 
           ELSE IF LS-MESSAGE-NUM = 5
                       MOVE LS-DATE (OFFSET - 4) TO POST-DATE
           ELSE IF LS-MESSAGE-NUM = 6
                       MOVE LS-DATE (OFFSET - 5) TO POST-DATE 
           ELSE IF LS-MESSAGE-NUM = 7
                       MOVE LS-DATE (OFFSET - 6) TO POST-DATE
           ELSE IF LS-MESSAGE-NUM = 8
                       MOVE LS-DATE (OFFSET - 7) TO POST-DATE
           ELSE IF LS-MESSAGE-NUM = 9
                       MOVE LS-DATE (OFFSET - 8) TO POST-DATE
           ELSE IF LS-MESSAGE-NUM = 10
                       MOVE LS-DATE (OFFSET - 9) TO POST-DATE                       
           END-IF.

           END FUNCTION DISPLAY-MESSAGE-DATE.
           