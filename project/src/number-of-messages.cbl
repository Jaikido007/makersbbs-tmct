       IDENTIFICATION DIVISION.
       PROGRAM-ID. number-of-messages.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT MESSAGES-FILE ASSIGN TO "messages.dat"
             ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD MESSAGES-FILE.
           01 RC-ID PIC XXX.
           WORKING-STORAGE SECTION.
           01 WS-COUNTER PIC 999.
           01 WS-FILE-IS-ENDED PIC 9 VALUE 0.
           01 WS-SUPPRESS-ZEROS PIC Z(5).
           LINKAGE SECTION.
           01 LS-RETURN-COUNTER PIC 999.
       PROCEDURE DIVISION USING LS-RETURN-COUNTER.
      ******************************************************************
      *********************----ABOUT THIS FILE---***********************
      *    This is a simple line counter program that rund through     *
      *    the messages.dat file and counts the entries. This then     *
      *    returns an integer value to the calling program.            *
      ****************************************************************** 

      ****************************************************************** 
      ***********-----OPENING OF THE FILE TO READ FROM-----*************
      ****************************************************************** 
           
           OPEN INPUT MESSAGES-FILE.

      ****************************************************************** 
      ***********-----LOOPING AS IT GOES THROUGH EACH LINE-----*********
      ****************************************************************** 

           PERFORM UNTIL WS-FILE-IS-ENDED = 1
             READ MESSAGES-FILE
             NOT AT END
               COMPUTE WS-COUNTER = WS-COUNTER + 1
             AT END MOVE 1 TO WS-FILE-IS-ENDED
           END-PERFORM.

      ****************************************************************** 
      ************-----FORMATTING DATA TO BE SENT BACK-----*************
      ****************************************************************** 
           
           MOVE WS-COUNTER TO LS-RETURN-COUNTER.
           MOVE WS-COUNTER TO WS-SUPPRESS-ZEROS.
           MOVE WS-SUPPRESS-ZEROS TO LS-RETURN-COUNTER.
           MOVE FUNCTION TRIM(LS-RETURN-COUNTER) TO LS-RETURN-COUNTER.

      ****************************************************************** 
      *************-----RESETTING FLAGS FOR FUTURE CALLS-----***********
      ****************************************************************** 
           
           MOVE 0 TO WS-COUNTER.
           MOVE 0 TO WS-FILE-IS-ENDED.

           CLOSE MESSAGES-FILE.

      ******************************************************************
      