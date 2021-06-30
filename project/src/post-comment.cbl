       IDENTIFICATION DIVISION.
       PROGRAM-ID. post-comment.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-COMMENTS-FILE ASSIGN TO 'comments.dat'
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-COMMENTS-FILE.
           01 RC-FILE-ENTRY.
               05 RC-ID PIC 999.
               05 RC-AUTHOR PIC X(16).
               05 RC-DATE-POST PIC X(10).
               05 RC-COMMENT PIC X(50).

           WORKING-STORAGE SECTION.
           01 MESSAGE-LINES PIC 999.
           01 ID-FIND PIC 999.

           01 FORMATTED-DATE-TIME.
               05 WS-DAY PIC XX.
               05 FILLER PIC X VALUE '-'.
               05 WS-MONTH PIC XX.
               05 FILLER PIC X VALUE '-'.
               05 WS-YEAR PIC X(4).

           01 WS-POST-COM-TBL.
               05 POST-ID PIC 999.
               05 WS-POST-COMMENT-AUTHOR PIC X(16).
               05 WS-POST-COMMENT-DATE PIC X(10).
               05 WS-WRITE-COMMENT PIC X(50).

           LINKAGE SECTION.
           01 MSG-SELECT PIC 999.

           01 POST-COM-TBL.
               05 POST-COMMENT-AUTHOR PIC X(16).
               05 POST-COMMENT-DATE PIC X(10).
               05 WRITE-COMMENT PIC X(50).

       PROCEDURE DIVISION USING MSG-SELECT, POST-COM-TBL.

           CALL 'number-of-file-lines' USING MESSAGE-LINES.

      ******************************************************************
      ****************------FORMATTING DATE TIME------******************
      ******************************************************************
           
           MOVE POST-COMMENT-DATE(1:4) TO WS-YEAR.
           MOVE POST-COMMENT-DATE(6:2) TO WS-MONTH.
           MOVE POST-COMMENT-DATE(9:2) TO WS-DAY.

           MOVE FORMATTED-DATE-TIME TO POST-COMMENT-DATE.

      ******************************************************************
           
           COMPUTE POST-ID = MESSAGE-LINES - MSG-SELECT + 1.

          *>  MOVING ALL IMPORTED DATA TO THE TABLE THAT HAS A SECTION
          *> FOR THE ID.

           MOVE POST-COMMENT-AUTHOR TO WS-POST-COMMENT-AUTHOR.
           MOVE POST-COMMENT-DATE TO WS-POST-COMMENT-DATE.
           MOVE WRITE-COMMENT TO WS-WRITE-COMMENT.

          *>  Opening the comments file and writing to it:

           OPEN EXTEND F-COMMENTS-FILE.
           MOVE WS-POST-COM-TBL TO RC-FILE-ENTRY.
           WRITE RC-FILE-ENTRY.
           CLOSE F-COMMENTS-FILE.


