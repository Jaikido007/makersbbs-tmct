       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION REPLACE-LETTER.

       DATA DIVISION.
           WORKING-STORAGE SECTION.

          *>  01 MESSAGE-LINES PIC 999.
           
          *>  01 COMMENT-TOTAL-TABLE.
          *>      05 COM-TOTAL-ENTRY OCCURS 1 TO 999 TIMES
          *>        DEPENDING ON MESSAGE-LINES.
          *>          10 SUM-COMMENTS PIC ZZZZ.

          *>  01 WS-TABLE.
          *>      05 WS-ENTRY OCCURS 10 TO 999 TIMES DEPENDING ON
          *>          MESSAGE-LINES.
          *>          10 WS-ID PIC XXX.
          *>          10 WS-TITLE PIC X(50).
          *>          10 WS-CONTENT PIC X(300).
          *>          10 WS-USERNAME PIC X(16).
          *>          10 WS-DATE PIC X(10).

          *>  01 WS-USER-INPUT PIC X.
          *>  SCREEN SECTION.

          *>  01 TEST-SCREEN
          *>      BACKGROUND-COLOR IS 0.
          *>    05 BLANK SCREEN.
          *>    05 LINE 10 COL 10 VALUE 'THIS IS A TEST OF TEXT COLOUR'
          *>          FOREGROUND-COLOR IS 8
                  *>  .
       PROCEDURE DIVISION.
           CALL "server".
           GOBACK
           .
      *>  0100-TEST-SCREEN-SECTION.
      *>      DISPLAY TEST-SCREEN.
           
      *>      ACCEPT WS-USER-INPUT.
      *>      IF WS-USER-INPUT = 'Q' OR 'q' THEN
      *>        STOP RUN
      *>      END-IF.

      *>      PERFORM 0100-TEST-SCREEN-SECTION.
           

          *>  ----------------------TESTING----------------------------
           
          *>  CALL 'number-of-file-lines' USING MESSAGE-LINES.

          *>  CALL 'count-comments-posted' USING COMMENT-TOTAL-TABLE.
           
          *>  DISPLAY 'TESTING FINAL OUTPUT:' SUM-COMMENTS(24).

          *>  CALL 'count-comments-posted' USING COMMENT-TOTAL-TABLE.

          *>  DISPLAY 'TESTING FINAL OUTPUT:' SUM-COMMENTS(24).
           
          *>  CALL 'get-list-page-alt' USING WS-TABLE.

          *>  DISPLAY 'Testing table entries: '.
          *>  DISPLAY 'WS-ID: ' WS-ID(1).
          *>  DISPLAY 'WS-TITLE: ' WS-TITLE(1).
          *>  DISPLAY 'WS-CONTENT: ' WS-CONTENT(1).
          *>  DISPLAY 'WS-USERNAME: ' WS-USERNAME(1).
          *>  DISPLAY 'WS-DATE: ' WS-DATE(1).

          *>  DISPLAY 'END.'.
