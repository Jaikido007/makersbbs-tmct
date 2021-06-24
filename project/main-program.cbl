       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
          *>  01 NUM-COMMENTS PIC 9999.
          *>  01 ADDING-TEST PIC 999 VALUE 1.
          *>  01 RESULT PIC 9999.

          *>  01 WS-COMMENT-UPPER-LIMIT PIC 9999.

          *>  01 COMMENT-TABLE.
          *>      05 COMMENT-ENTRY OCCURS 1 TO 9999 TIMES 
          *>      DEPENDING ON NUM-COMMENTS.
          *>         *>  10 TEMP-ID PIC 999.
          *>          10 WS-AUTHOR PIC X(16).
          *>          10 WS-DATE PIC X(21).
          *>          10 WS-COMMENT PIC X(50).

          *>  01 MSG-SELECT PIC 999.

       PROCEDURE DIVISION.
           CALL "server".
           GOBACK
           .
          *>  ----------------------TESTING----------------------------

          *>  CALL 'num-comments' USING NUM-COMMENTS.
          *>  DISPLAY NUM-COMMENTS.

          *>  ADD ADDING-TEST TO NUM-COMMENTS.
          *>  MOVE NUM-COMMENTS TO RESULT.

          *>  DISPLAY 'ADDING A PIC 999 TO A PIC 9999...'.
          *>  DISPLAY 'RESULT: ' RESULT.
           

          *>  ---------------------------------------------------------

          *> ------------COMMENT TABLE TESTING SECTION---------------

          *>  MOVE 26 TO MSG-SELECT.

          *>  DISPLAY 'Message id for select is: ' MSG-SELECT.

          *>  DISPLAY 'Calling get-comment.cbl using ' MSG-SELECT.

          *>  CALL 'get-comment' USING COMMENT-TABLE MSG-SELECT.

          *>  DISPLAY 'Output result is: '.
           
          *>  DISPLAY COMMENT-TABLE.

          *> ---------------------------------------------------------

