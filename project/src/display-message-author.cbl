       IDENTIFICATION DIVISION.
       FUNCTION-ID. DISPLAY-MESSAGE-AUTHOR.
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
           01 POST-AUTHOR PIC X(10).
           01 OFFSET UNSIGNED-INT.

       PROCEDURE DIVISION USING OFFSET LS-MESSAGE-NUM LS-MSGS 
       RETURNING POST-AUTHOR.
           
           IF LS-MESSAGE-NUM = 1
                       MOVE LS-AUTHOR (OFFSET) TO POST-AUTHOR    
           ELSE IF LS-MESSAGE-NUM = 2
                       MOVE LS-AUTHOR (OFFSET - 1) TO POST-AUTHOR
           ELSE IF LS-MESSAGE-NUM = 3
                       MOVE LS-AUTHOR (OFFSET - 2) TO POST-AUTHOR 
           ELSE IF LS-MESSAGE-NUM = 4
                       MOVE LS-AUTHOR (OFFSET - 3) TO POST-AUTHOR 
           ELSE IF LS-MESSAGE-NUM = 5
                       MOVE LS-AUTHOR (OFFSET - 4) TO POST-AUTHOR
           ELSE IF LS-MESSAGE-NUM = 6
                       MOVE LS-AUTHOR (OFFSET - 5) TO POST-AUTHOR 
           ELSE IF LS-MESSAGE-NUM = 7
                       MOVE LS-AUTHOR (OFFSET - 6) TO POST-AUTHOR
           ELSE IF LS-MESSAGE-NUM = 8
                       MOVE LS-AUTHOR (OFFSET - 7) TO POST-AUTHOR
           ELSE IF LS-MESSAGE-NUM = 9
                       MOVE LS-AUTHOR (OFFSET - 8) TO POST-AUTHOR
           ELSE IF LS-MESSAGE-NUM = 10
                       MOVE LS-AUTHOR (OFFSET - 9) TO POST-AUTHOR                       
           END-IF.

           END FUNCTION DISPLAY-MESSAGE-AUTHOR.
           