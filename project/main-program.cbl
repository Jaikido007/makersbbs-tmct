       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.

       DATA DIVISION.
           WORKING-STORAGE SECTION.

           01 MESSAGE-LINES PIC 999.
           
           01 COMMENT-TOTAL-TABLE.
               05 COM-TOTAL-ENTRY OCCURS 1 TO 999 TIMES
                 DEPENDING ON MESSAGE-LINES.
                   10 SUM-COMMENTS PIC ZZZZ.

       PROCEDURE DIVISION.
           CALL "server".
           GOBACK
           .

          *>  ----------------------TESTING----------------------------
           
          *>  CALL 'number-of-file-lines' USING MESSAGE-LINES.

          *>  CALL 'count-comments-posted' USING COMMENT-TOTAL-TABLE.
           
          *>  DISPLAY 'TESTING FINAL OUTPUT:' SUM-COMMENTS(24).

          *>  CALL 'count-comments-posted' USING COMMENT-TOTAL-TABLE.

          *>  DISPLAY 'TESTING FINAL OUTPUT:' SUM-COMMENTS(24).
           
