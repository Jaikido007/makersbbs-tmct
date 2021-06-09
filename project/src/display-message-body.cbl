       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-MESSAGE-BODY.
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
           01 BODY PIC X(500).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-MESSAGE-NUM LS-MSGS 
       RETURNING BODY.
           
           IF LS-MESSAGE-NUM = 1
                       MOVE LS-BODY(OFFSET) TO BODY    
           ELSE IF LS-MESSAGE-NUM = 2
                       MOVE LS-BODY(OFFSET - 1) TO BODY
           ELSE IF LS-MESSAGE-NUM = 3
                       MOVE LS-BODY(OFFSET - 2) TO BODY 
           ELSE IF LS-MESSAGE-NUM = 4
                       MOVE LS-BODY(OFFSET - 3) TO BODY 
           ELSE IF LS-MESSAGE-NUM = 5
                       MOVE LS-BODY(OFFSET - 4) TO BODY
           ELSE IF LS-MESSAGE-NUM = 6
                       MOVE LS-BODY(OFFSET - 5) TO BODY 
           ELSE IF LS-MESSAGE-NUM = 7
                       MOVE LS-BODY(OFFSET - 6) TO BODY
           ELSE IF LS-MESSAGE-NUM = 8
                       MOVE LS-BODY(OFFSET - 7) TO BODY
           ELSE IF LS-MESSAGE-NUM = 9
                       MOVE LS-BODY(OFFSET - 8) TO BODY
           ELSE IF LS-MESSAGE-NUM = 10
                       MOVE LS-BODY(OFFSET - 9) TO BODY                       
           END-IF.

           END FUNCTION DISPLAY-MESSAGE-BODY.
           