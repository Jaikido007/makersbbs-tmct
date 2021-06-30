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
             05 RC-DATE-POST PIC X(10).
             05 RC-COMMENT PIC X(50).

           WORKING-STORAGE SECTION.
           01 MESSAGE-LINES PIC 999.
           01 NUM-COMMENTS PIC 9999.

           01 COUNTER PIC 999.
           01 ID-FIND PIC 999.

           01 COM-INDEX PIC 9999 VALUE 1.

           01 TEMP-TABLE.
               05 WS-ENTRY OCCURS 1 TO 9999 TIMES 
               DEPENDING ON NUM-COMMENTS.
                   10 TEMP-AUTHOR PIC X(16).
                   10 TEMP-DATE PIC X(10).
                   10 TEMP-COMMENT PIC X(50).

           01 WS-FILE-END PIC 9 VALUE 0.
           LINKAGE SECTION.
           01 FINAL-TABLE.
               05 LS-ENTRY OCCURS 1 TO 9999 TIMES
                 DEPENDING ON NUM-COMMENTS.
                   10 LS-AUTHOR PIC X(16).
                   10 LS-DATE PIC X(10).
                   10 LS-COMMENT PIC X(50).
    
           01 MSG-SELECT PIC 999.

       PROCEDURE DIVISION USING FINAL-TABLE, MSG-SELECT.
           
           CALL 'number-of-messages' USING MESSAGE-LINES.
           CALL 'num-comments' USING NUM-COMMENTS.
      ******************************************************************
      *********************----ABOUT THIS FILE---***********************
      *  This file takes a table and a number as an argument.          *
      *  The incoming number argument from the calling program is      *
      *  the index number passed through related to the messages list  *
      *  on the server.cbl file. The original index is flipped, so     *
      *  taking                                                        *
      *  the NUMBER OF MESSAGES (MESSAGE-LINES) - (MSG-SELECT) + 1 to  *
      *  get the original index. Then we use this unflipped index to   *
      *  find all comments posted with this index, then push the data  *
      *  to a table for this program to return and for the server.cbl  *
      *  to use                                                        * 
      *                                                                * 
      *    How to find original index:                                 * 
      *    Number of messages - flipped index number + 1 to get the    * 
      *    original pre flipped index.                                 * 
      *    Example: 26(msglines) - 25(flipped indx) + 1 = 2.           *                                          *
      ****************************************************************** 
      ***********-----FINDING ORIGINAL INDEX OF MESSAGES-----***********
      ******************************************************************   

           COMPUTE ID-FIND = MESSAGE-LINES - MSG-SELECT + 1.
      
      ******************************************************************
      **********-----WIPE INCOMING TABLE FOR NEW DATA-----**************
      ******************************************************************

           PERFORM UNTIL COM-INDEX = NUM-COMMENTS
             MOVE SPACES TO WS-ENTRY(COM-INDEX)
             ADD 1 TO COM-INDEX
           END-PERFORM.
           
           MOVE TEMP-TABLE TO FINAL-TABLE.
           MOVE 1 TO COM-INDEX.

      ******************************************************************
      *********-----READING AND WRITING OF REQUESTED DATA-----**********
      ******************************************************************
           
           OPEN INPUT F-COMMENTS-FILE.

           PERFORM UNTIL WS-FILE-END = 1
             READ F-COMMENTS-FILE
             NOT AT END

               IF ID-FIND = RC-ID
                 ADD 1 TO COUNTER
                 MOVE RC-AUTHOR TO TEMP-AUTHOR(COUNTER)
                 MOVE RC-DATE-POST TO TEMP-DATE(COUNTER)
                 MOVE RC-COMMENT TO TEMP-COMMENT(COUNTER)
               END-IF

             AT END MOVE 1 TO WS-FILE-END
           END-PERFORM.

           CLOSE F-COMMENTS-FILE.
       
      ******************************************************************
      **********-----EXPORTING NEW DATA TO CALLING PROGRAM-----*********
      ******************************************************************

           MOVE TEMP-TABLE TO FINAL-TABLE.
    
      ******************************************************************
      *************-----RESET VALUES FOR FUTURE CALLS-----**************
      ******************************************************************
         
           SUBTRACT 1 FROM WS-FILE-END.
           MOVE 0 TO COUNTER.

      ******************************************************************         
