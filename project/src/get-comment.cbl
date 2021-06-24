       IDENTIFICATION DIVISION.
       PROGRAM-ID. get-comment.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-COMMENTS-FILE ASSIGN TO "comments.dat"
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD F-COMMENTS-FILE.
           01 RC-FILE-ENTRY.
             05 RC-ID PIC 999.
             05 RC-AUTHOR PIC X(16).
             05 RC-DATE-POST PIC X(21).
             05 RC-COMMENT PIC X(50).

           WORKING-STORAGE SECTION.
           01 MESSAGE-LINES PIC 999.
           01 NUM-COMMENTS PIC 9999.

           01 COUNTER PIC 999.
           01 MATCHED-COMMENTS PIC 999 VALUE 0.
           01 ID-FIND PIC 999.

           01 COM-INDEX PIC 9999 VALUE 1.
           01 WIPE PIC X(87) VALUE SPACES.

           01 TEMP-TABLE.
               05 WS-ENTRY OCCURS 1 TO 9999 TIMES 
               DEPENDING ON NUM-COMMENTS.
                  *>  10 TEMP-ID PIC 999.
                   10 TEMP-AUTHOR PIC X(16).
                   10 TEMP-DATE PIC X(21).
                   10 TEMP-COMMENT PIC X(50).

           01 WS-FILE-END PIC 9 VALUE 0.
           LINKAGE SECTION.
           01 FINAL-TABLE.
               05 LS-ENTRY OCCURS 1 TO 9999 TIMES
                 DEPENDING ON NUM-COMMENTS.
                  *>  10 LS-ID PIC 999.
                   10 LS-AUTHOR PIC X(16).
                   10 LS-DATE PIC X(21).
                   10 LS-COMMENT PIC X(50).
    
           01 MSG-SELECT PIC 999.

          *>  01 MATCHED-ENTRIES PIC 9(4).

       PROCEDURE DIVISION USING FINAL-TABLE, MSG-SELECT.
           
           CALL 'number-of-file-lines' USING MESSAGE-LINES.
           CALL 'num-comments' USING NUM-COMMENTS.


          *>  INFO:
          *> This file takes a table and a number as arguments.
          *> Using these values, this program looks through the
          *> comments.dat file for matching cases. If it finds any it
          *> then pushes the line to a temporary table. After the file
          *> has finished it exports the table to the calling program
          *> where the data can then be used elsewhere.

          *>  --------------------------------------------------------

          *>  How to find original index:
          *> Number of messages - flipped index number + 1 to get the
          *> original pre flipped index.
          *> Example: 26(msglines) - 25(flipped indx) + 1 = 2.

           COMPUTE ID-FIND = MESSAGE-LINES - MSG-SELECT + 1.

          *>  Wipe the table so no unrequested data is shown:
           
           PERFORM UNTIL COM-INDEX = NUM-COMMENTS
             MOVE SPACES TO WS-ENTRY(COM-INDEX)
             ADD 1 TO COM-INDEX
           END-PERFORM
           .

           MOVE TEMP-TABLE TO FINAL-TABLE.

           MOVE 1 TO COM-INDEX.

          *>  STARTING FILE READING AND WRITING REQUESTED DATE TO TABLE:
           
           OPEN INPUT F-COMMENTS-FILE.

           PERFORM UNTIL WS-FILE-END = 1
             READ F-COMMENTS-FILE
             NOT AT END

              *>  DISPLAY 'DEBUG - ID-FIND is: ' ID-FIND
              *>  DISPLAY 'DEBUG - RC-ID is: ' RC-ID

               IF ID-FIND = RC-ID
                *>  DISPLAY 'Inside the if block, condition is true'
                 ADD 1 TO COUNTER
                 MOVE RC-AUTHOR TO TEMP-AUTHOR(COUNTER)
                 MOVE RC-DATE-POST TO TEMP-DATE(COUNTER)
                 MOVE RC-COMMENT TO TEMP-COMMENT(COUNTER)
               END-IF

             AT END MOVE 1 TO WS-FILE-END
           END-PERFORM
           .

           CLOSE F-COMMENTS-FILE.

          *>  Exporting local table to calling program vv.
           MOVE TEMP-TABLE TO FINAL-TABLE.
           
          *> vv Reset file end flag for future calls vv
           SUBTRACT 1 FROM WS-FILE-END.
           MOVE 0 TO COUNTER.



          *>  Notes:
          *> Remember to set the server comment-table limit to be
          *> dependent on the number of comments there are. Without it
          *> the table will not show the right amount of entries.



           
