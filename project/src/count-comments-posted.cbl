       IDENTIFICATION DIVISION.
       PROGRAM-ID. count-comments-posted.
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

           WORKING-STORAGE SECTION.
           01 MESSAGE-LINES PIC 999.
           01 COMMENT-COUNTER PIC 999.
           01 ID-FIND PIC 999.

           01 TEMP-TABLE.
               05 WS-ENTRY OCCURS 1 TO 999 TIMES 
               DEPENDING ON MESSAGE-LINES.
                   10 WS-SUM-COMMENTS PIC 9999.

           01 WS-FILE-END PIC 9 VALUE 0.
           01 MSG-SELECT PIC 999.

           LINKAGE SECTION.

           01 COMMENT-TOTAL-TABLE.
               05 COM-TOTAL-ENTRY OCCURS 1 TO 999 TIMES
                 DEPENDING ON MESSAGE-LINES.
                   10 SUM-COMMENTS PIC 9999.
    
       PROCEDURE DIVISION USING COMMENT-TOTAL-TABLE.

      ******************************************************************
      *********************----ABOUT THIS FILE---***********************
      *    This program looks through the comments.dat file and looks  *
      *    for  all posts starting with the same index, counts how     *
      *    many of them there are, then puts them to a table to be     *
      *    exported using the same original indexes as the message     *
      *    posts would belong to.                                      *
      ****************************************************************** 

      ****************************************************************** 
      *************-----GET NUMBER OF CURRENT MESSAGES-----*************
      ****************************************************************** 

           CALL 'number-of-messages' USING MESSAGE-LINES.

      ****************************************************************** 
      ********-----SET MESSAGE-SELECT TO TOTAL OF MESSAGES-----*********
      ****************************************************************** 

           MOVE MESSAGE-LINES TO MSG-SELECT.

      ****************************************************************** 
      *************-----FIND ORIGINAL INDEX OF MESSAGES-----************
      ****************************************************************** 

           COMPUTE ID-FIND = MESSAGE-LINES - MSG-SELECT + 1.
           
      ****************************************************************** 
      **************-----CLEAR EXISTING TABLE DATA-----*****************
      ******************************************************************

           MOVE SPACES TO TEMP-TABLE.
           MOVE TEMP-TABLE TO COMMENT-TOTAL-TABLE.
      
      ****************************************************************** 
      *******-----NESTED LOOP TO FIND EACH COMMENT WITH SAME-----******
      *******-----INDEXES AND PUT THE COUNT OF THEM TO A TABLE-----*****
      ****************************************************************** 

           PERFORM UNTIL ID-FIND > MESSAGE-LINES
             OPEN INPUT F-COMMENTS-FILE

             *>  RESET FILE END FLAG:
             MOVE 0 TO WS-FILE-END

             *>  RESET COUNTER:
             MOVE 0 TO COMMENT-COUNTER

               PERFORM UNTIL WS-FILE-END = 1
                 READ F-COMMENTS-FILE
                 NOT AT END
                   IF ID-FIND = RC-ID
                     ADD 1 TO COMMENT-COUNTER
                   END-IF
                 AT END MOVE 1 TO WS-FILE-END             
               END-PERFORM
               
             MOVE COMMENT-COUNTER TO WS-SUM-COMMENTS(MSG-SELECT)

             CLOSE F-COMMENTS-FILE

             SUBTRACT 1 FROM MSG-SELECT
      
      ****************************************************************** 
      ****************-----GET NEW ID TO SEARCH FOR-----****************
      ****************************************************************** 

             COMPUTE ID-FIND = MESSAGE-LINES - MSG-SELECT + 1

           END-PERFORM.

      ****************************************************************** 
      *************-----EXPORT TABLE TO CALLING PROGRAM-----************
      ****************************************************************** 

           MOVE TEMP-TABLE TO COMMENT-TOTAL-TABLE.
      
      ******************************************************************
