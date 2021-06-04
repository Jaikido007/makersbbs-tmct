       IDENTIFICATION DIVISION.
       PROGRAM-ID. server.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           REPOSITORY.
               FUNCTION GENERATE-MESSAGE-NUM.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT F-MESSAGE-FILE ASSIGN TO "messages.dat"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
             FD F-MESSAGE-FILE.
             01 MESSAGES.
               05 MESSAGE-TITLE PIC X(60).
               05 MESSAGE-BODY PIC X(500).

           WORKING-STORAGE SECTION.
           01 USER-NAME PIC X(10).
           01 MENU-CHOICE PIC X.
           01 COUNTER UNSIGNED-INT.
           01 NUM UNSIGNED-INT.
           01 NUM1 UNSIGNED-INT.
           01 NUM2 UNSIGNED-INT.
           01 NUM3 UNSIGNED-INT.
           01 NUM4 UNSIGNED-INT.
           01 NUM5 UNSIGNED-INT.
           01 NUM6 UNSIGNED-INT.
           01 NUM7 UNSIGNED-INT.
           01 NUM8 UNSIGNED-INT.
           01 NUM9 UNSIGNED-INT.
           01 NUM10 UNSIGNED-INT.
           01 MESSAGE-CHOICE PIC X.
           01 WS-COUNTER PIC 99.
           01 WS-FILE-IS-ENDED PIC 9.
           01 WS-MESSAGES.
               05 WS-MESSAGE OCCURS 10 TIMES
               ASCENDING KEY IS WS-TITLE
               INDEXED BY MSG-IDX.
                   10 WS-TITLE PIC X(60).
           01 MESS-TITLE PIC X(60).
           
           LINKAGE SECTION.
           01 LS-COUNTER UNSIGNED-INT.
           01 LS-NUM UNSIGNED-INT.
           01 LS-MESSAGE PIC X(60).
           SCREEN SECTION.
           01 LOGIN-SCREEN.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "What's your name?".
             05 USER-NAME-FIELD LINE 6 COLUMN 10 PIC X(10)
                USING USER-NAME.

           01 MENU-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Welcome, ".
             05 LINE 4 COLUMN 19 PIC X(10) USING USER-NAME.
             05 LINE 8 COLUMN 10 VALUE "(n) Nothing".
             05 LINE 8 COLUMN 80 VALUE "(m) Message board".
             05 LINE 8 COLUMN 30 VALUE "(l) Logout".
             05 LINE 8 COLUMN 60 VALUE "(q) Quit".
             05 LINE 20 COLUMN 10 VALUE "Pick: ".
             05 MENU-CHOICE-FIELD LINE 20 COLUMN 16 PIC X
                USING MENU-CHOICE.

           01 MESSAGEBOARD-SCREEN
             BACKGROUND-COLOR IS 8.
             05 BLANK SCREEN.
             05 LINE 2 COLUMN 10 VALUE "Makers BBS".
             05 LINE 4 COLUMN 10 VALUE "Here are the last 10 messages:".
             05 LINE 6 COLUMN 10 VALUE "1.".
             05 LINE 6 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM1).
             05 LINE 7 COLUMN 10 VALUE "2.".
             05 LINE 7 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM2).
             05 LINE 8 COLUMN 10 VALUE "3.".
             05 LINE 8 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM3).
             05 LINE 9 COLUMN 10 VALUE "4.".
             05 LINE 9 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM4).
             05 LINE 10 COLUMN 10 VALUE "5.".
             05 LINE 10 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM5).
             05 LINE 11 COLUMN 10 VALUE "6.".
             05 LINE 11 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM6).
             05 LINE 12 COLUMN 10 VALUE "7.".
             05 LINE 12 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM7).
             05 LINE 13 COLUMN 10 VALUE "8.".
             05 LINE 13 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM8).
             05 LINE 14 COLUMN 10 VALUE "9.".
             05 LINE 14 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM9).
             05 LINE 15 COLUMN 10 VALUE "10.".
             05 LINE 15 COLUMN 14 PIC X(60) USING WS-MESSAGE(NUM10).
             05 LINE 17 COLUMN 10 VALUE "( ) Read the full message by ".
      *         "number".
            05 LINE 18 COLUMN 10 VALUE "(m) Post a message of your ".
      *        "own".
             05 LINE 19 COLUMN 10 VALUE "(n) Next page".
             05 LINE 19 COLUMN 30 VALUE "(p) Previous page".
             05 LINE 19 COLUMN 60 VALUE "(q) Go back".
             05 LINE 21 COLUMN 10 VALUE "Pick: ".
             05 MESSAGE-CHOICE-FIELD LINE 21 COLUMN 16 PIC X
                USING MESSAGE-CHOICE.

       PROCEDURE DIVISION.
      




       0110-DISPLAY-LOGIN.
           INITIALIZE USER-NAME.
           DISPLAY LOGIN-SCREEN.
           ACCEPT USER-NAME-FIELD.
           PERFORM 0120-DISPLAY-MENU.

       0120-DISPLAY-MENU.
           INITIALIZE MENU-CHOICE.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-CHOICE-FIELD.
           IF MENU-CHOICE = "q" THEN
           STOP RUN
           ELSE IF MENU-CHOICE = "l" THEN
           PERFORM 0110-DISPLAY-LOGIN
           ELSE IF MENU-CHOICE = "n" THEN
           PERFORM 0120-DISPLAY-MENU
           ELSE IF MENU-CHOICE = 'm' THEN
             PERFORM 0130-DISPLAY-MESSAGEBOARD
           END-IF. 
       

       0130-DISPLAY-MESSAGEBOARD.
           SET MSG-IDX TO 0.
           SET COUNTER TO 0.
           MOVE 0 TO WS-FILE-IS-ENDED.
           OPEN INPUT F-MESSAGE-FILE.
           PERFORM UNTIL WS-FILE-IS-ENDED = 1
               READ F-MESSAGE-FILE
                   NOT AT END
                       ADD 1 TO MSG-IDX
                       ADD 1 TO COUNTER
                    IF COUNTER = 1                     
                       MOVE MSG-IDX TO NUM1
                    END-IF
                    IF COUNTER = 2                       
                       MOVE MSG-IDX TO NUM2
                     END-IF
                    IF COUNTER = 3                       
                       MOVE MSG-IDX TO NUM3
                    END-IF
                    IF COUNTER = 4                       
                       MOVE MSG-IDX TO NUM4
                    END-IF
                    IF COUNTER = 5                       
                       MOVE MSG-IDX TO NUM5
                    END-IF
                    IF COUNTER = 6                       
                       MOVE MSG-IDX TO NUM6
                    END-IF
                    IF COUNTER = 7                       
                       MOVE MSG-IDX TO NUM7
                    END-IF
                    IF COUNTER = 8                       
                       MOVE MSG-IDX TO NUM8
                    END-IF
                    IF COUNTER = 9                       
                       MOVE MSG-IDX TO NUM9
                    ELSE
                        MOVE MSG-IDX TO NUM10
                    END-IF
      *           MOVE FUNCTION GENERATE-MESSAGE-NUM(COUNTER) TO NUM 
                 MOVE MESSAGE-TITLE TO WS-MESSAGE(MSG-IDX)
                           
                   AT END 
                       MOVE 1 TO WS-FILE-IS-ENDED 
               END-READ 
           END-PERFORM.
           CLOSE F-MESSAGE-FILE.
       

           INITIALIZE MESSAGE-CHOICE.
      *     MOVE FUNCTION GENERATE-MESSAGE-NUM TO MESS-TITLE.
           DISPLAY MESSAGEBOARD-SCREEN.
           ACCEPT MESSAGE-CHOICE-FIELD.
           IF MESSAGE-CHOICE = "q" THEN 
               PERFORM 0120-DISPLAY-MENU
           END-IF.
